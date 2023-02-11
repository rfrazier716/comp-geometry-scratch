use std::collections::HashMap;

use crate::allocator::{Allocator, Key};

#[derive(Debug, Copy, Clone, PartialEq, Default, std::cmp::Eq, Hash)]
struct VertexKey(Key);

#[derive(Debug, Copy, Clone, PartialEq, Default, std::cmp::Eq, Hash)]
struct HalfEdgeKey(Key);

#[derive(Debug, Copy, Clone, PartialEq, Default, std::cmp::Eq, Hash)]
struct FaceKey(Key);

#[derive(Default, Debug)]
struct Point {
    pub x: f64,
    pub y: f64,
    pub z: f64,
}

impl Point {
    pub fn new(x: f64, y: f64, z: f64) -> Self {
        Point { x, y, z }
    }
}

#[derive(Default, Debug)]
struct Vertex {
    position: Point,                    // the 3D coordinate of the point
    incident_edge: Option<HalfEdgeKey>, // Refers to a single edge incident to the point
}

#[derive(Default, Debug)]

struct HalfEdge {
    origin: Option<VertexKey>,
    twin: Option<HalfEdgeKey>,
    incident_face: Option<FaceKey>,
    next: Option<HalfEdgeKey>,
    prev: Option<HalfEdgeKey>,
}

#[derive(Default, Debug)]
struct Face {
    primary_edge: Option<HalfEdgeKey>,
}

struct DCEL {
    vertices: Allocator<Vertex>,
    edges: Allocator<HalfEdge>,
    faces: Allocator<Face>,
}

impl DCEL {
    pub fn new() -> Self {
        return DCEL {
            vertices: Allocator::new(),
            edges: Allocator::new(),
            faces: Allocator::new(),
        };
    }

    pub fn tetrahedron() -> Self {
        // DCEL for a Cube Centered at the Origin
        let mut dcel = Self::new();

        // create our points that are the corners of the cube
        let point_keys: Vec<VertexKey> = [
            ((8.0 / 9.0f64).sqrt(), 0.0, -1.0 / 3.0),
            (-(2.0 / 9.0f64).sqrt(), (2.0 / 3.0f64).sqrt(), -1.0 / 3.0),
            (-(2.0 / 9.0f64).sqrt(), -(2.0 / 3.0f64).sqrt(), -1.0 / 3.0),
            (0.0, 0.0, 1.0),
        ]
        .into_iter()
        .map(|(x, y, z)| {
            VertexKey(dcel.vertices.insert(Vertex {
                position: Point::new(x, y, z),
                incident_edge: None,
            }))
        })
        .collect();

        // faces are references to the vertices
        let face_indices = [[0, 1, 2], [2, 1, 3], [0, 2, 3], [0, 3, 1]];

        // create a hashmap to keep track of which half-edges already exist
        let mut created_edges: HashMap<(usize, usize), HalfEdgeKey> = HashMap::new();

        for face_vertices in face_indices {
            // Create a new face in our DCEL
            let face = FaceKey(dcel.faces.insert(Face { primary_edge: None }));

            let edges_to_add = (0..3).map(|i| match i {
                2 => (face_vertices[i], face_vertices[0]),
                _ => (face_vertices[i], face_vertices[i + 1]),
            });
            // if we've seen any edges from point A->B already, we need to flip the orientation of the triangle
            // otherwise we'll have duplicate half-edges
            let requires_flip = edges_to_add
                .clone()
                .fold(false, |prev, cur| prev || created_edges.contains_key(&cur));

            //Create our half edges and keep the keys so we can reference them later
            let edge_keys: Vec<HalfEdgeKey> = edges_to_add
                .map(|(i1, i2)| if requires_flip { (i2, i1) } else { (i1, i2) }) // flip order if required
                .map(|(p_origin, p_destination)| {
                    // insert our edge into the dcel
                    let edge_key = HalfEdgeKey(dcel.edges.insert(HalfEdge {
                        origin: Some(point_keys[p_origin]),
                        twin: created_edges.get(&(p_destination, p_origin)).copied(),
                        incident_face: Some(face),
                        next: None,
                        prev: None,
                    }));

                    // track it in our map so we can tell when its twin is created
                    created_edges.insert((p_origin, p_destination), edge_key);

                    // if this edges twin exists, update its twin reference
                    if let Some(twin_key) = created_edges.get(&(p_destination, p_origin)) {
                        if let Some(twin) = dcel.edges.get_mut(&twin_key.0) {
                            twin.twin = Some(edge_key);
                        }
                    }

                    // If this edges origin point does not have an incident edge yet, create it
                    if let Some(vertex) = dcel.vertices.get_mut(&point_keys[p_origin].0) {
                        vertex.incident_edge.get_or_insert(edge_key);
                    }

                    edge_key
                })
                .collect(); // collect into a vec

            // now link together next and prev elements
            edge_keys.iter().fold(
                Option::<HalfEdgeKey>::None,
                |prev_edge_key, current_edge_key| {
                    // point the previous to the current
                    if let Some(previous_edge) =
                        prev_edge_key.and_then(|prev_key| dcel.edges.get_mut(&prev_key.0))
                    {
                        previous_edge.next = Some(*current_edge_key);
                    }

                    // point the current to the previous
                    if let Some(current_edge) = dcel.edges.get_mut(&current_edge_key.0) {
                        current_edge.prev = prev_edge_key;
                    }

                    Some(*current_edge_key) // for the next entry
                },
            );

            // we have to manually stitch the first and last edges togeter
            if let Some(edge) = dcel.edges.get_mut(&edge_keys[0].0) {
                edge.prev = edge_keys.last().copied();
            }
            if let Some(edge) = dcel.edges.get_mut(&edge_keys[2].0) {
                edge.next = edge_keys.first().copied();
            }

            // finally - have our created face point to the first edge in our list
            if let Some(face) = dcel.faces.get_mut(&face.0) {
                face.primary_edge = Some(edge_keys[0]);
            }
        }
        dcel
    }
}

impl DCEL {
    pub fn find_cycle(&self, face_key: FaceKey) -> Vec<HalfEdgeKey> {
        // Returns a vector of edge keys that form a cycle around a face
        todo!()
    }

    pub fn find_outgoing_edges(&self, vertex: VertexKey) -> Vec<HalfEdgeKey> {
        todo!()
    }
}


#[cfg(test)]

mod tests {
    use super::*;
    use std::collections::HashSet;
    #[test]
    fn test_tetrahedron_vertices() {
        let shape = DCEL::tetrahedron();

        // we should have four faces, four points, and 12 half-edges\

        assert_eq!(shape.vertices.len(), 4);

        // Every vertex should be associated with a unique edge
        let incident_edge_set: HashSet<HalfEdgeKey> = HashSet::from_iter(
            shape
                .vertices
                .iter_values()
                .filter_map(|vertex| vertex.incident_edge),
        );
        assert_eq!(incident_edge_set.len(), 4); // a set will remove duplicate edges
    }

    #[test]
    fn test_tetrahedron_faces() {
        let shape = DCEL::tetrahedron();
        assert_eq!(shape.faces.len(), 4);

        // Every Face should have a unique Edge
        let face_incident_edge_set: HashSet<HalfEdgeKey> = HashSet::from_iter(
            shape
                .faces
                .iter_values()
                .filter_map(|face| face.primary_edge),
        );
        assert_eq!(face_incident_edge_set.len(), 4);
    }
    #[test]
    fn test_tetrahedron_edges() {
        let shape = DCEL::tetrahedron();
        assert_eq!(shape.edges.len(), 12); // structure should be packed so we're fine

        for edge_key in shape.edges.iter_keys() {
            // for each edge, the twin of the twin should be the original edge
            let twin_key = shape
                .edges
                .get(&edge_key)
                .and_then(|edge| edge.twin)
                .unwrap();
            let twin_of_twin_key = shape
                .edges
                .get(&twin_key.0)
                .and_then(|edge| edge.twin)
                .unwrap();
            assert_eq!(edge_key, twin_of_twin_key.0);

            // for each edge, the next of the previous should be the original edge
            let prev_key = shape.edges.get(&edge_key).and_then(|x| x.prev).unwrap();
            let next_of_prev_key = shape
                .edges
                .get(&prev_key.0)
                .and_then(|prev_edge| prev_edge.next)
                .unwrap();
            assert_eq!(edge_key, next_of_prev_key.0);

            // Since Every Face is a Triangle, if we cycle an edge next three times or previous three times we should get the original edge
            let third_key_moving_forward = (0..3).fold(edge_key, |prev_key, _| {
                shape.edges.get(&prev_key).and_then(|prev_edge| prev_edge.next).unwrap().0
            });
            let third_key_moving_backwards = (0..3).fold(edge_key, |prev_key, _| {
                shape.edges.get(&prev_key).and_then(|prev_edge| prev_edge.prev).unwrap().0
            });
            assert_eq!(edge_key, third_key_moving_forward);
            assert_eq!(edge_key, third_key_moving_backwards);
        }
    }

    #[test]
    fn test_edge_cycle_references_same_face() {
        let shape = DCEL::tetrahedron();
        for edge_key in shape.edges.iter_keys().map(|x| HalfEdgeKey(x)) {
            // The face key for every edge in a cycle should be the same and none-null
            let edge_face = shape
                .edges
                .get(&edge_key.0)
                .and_then(|edge| edge.incident_face)
                .unwrap();

            // this cycles across every edge and checks that they all reference the same face as the edge under test
            let faces_equal = (0..3)
                .scan(edge_key, |prev_key, _| {
                    shape
                        .edges
                        .get(&prev_key.0) // get the previous edge
                        .and_then(|x| x.next) // find the next edge key
                        .and_then(|edge_key| {
                            *prev_key = edge_key;
                            shape.edges.get(&edge_key.0)
                        }) // find the next edge
                        .and_then(|edge| edge.incident_face)
                })
                .fold(true, |valid, current_face| {
                    valid && (edge_face == current_face)
                });
            assert!(faces_equal);
        }
    }
}
