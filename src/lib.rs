pub mod allocator;

use std::collections::{HashMap, HashSet};
use wasm_bindgen::prelude::*;

use crate::allocator::{Allocator, Key};

#[derive(Debug, Copy, Clone, PartialEq, Default, std::cmp::Eq, Hash)]
struct VertexKey(Key);

#[derive(Debug, Copy, Clone, PartialEq, Default, std::cmp::Eq, Hash)]
struct HalfEdgeKey(Key);

#[derive(Debug, Copy, Clone, PartialEq, Default, std::cmp::Eq, Hash)]
struct FaceKey(Key);

#[derive(Default, Debug, Copy, Clone)]
pub struct Point {
    pub x: f32,
    pub y: f32,
    pub z: f32,
}

impl Point {
    pub fn new(x: f32, y: f32, z: f32) -> Self {
        Point { x, y, z }
    }
}

#[derive(Default, Debug, Copy, Clone)]
struct Vertex {
    position: Point,                    // the 3D coordinate of the point
    incident_edge: Option<HalfEdgeKey>, // Refers to a single edge incident to the point
}

#[derive(Default, Debug, Copy, Clone)]

struct HalfEdge {
    origin: Option<VertexKey>,
    twin: Option<HalfEdgeKey>,
    incident_face: Option<FaceKey>,
    next: Option<HalfEdgeKey>,
    prev: Option<HalfEdgeKey>,
}

#[derive(Default, Debug, Copy, Clone)]
struct Face {
    primary_edge: Option<HalfEdgeKey>,
}

#[derive(Clone)]
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
            ((8.0 / 9.0f32).sqrt(), 0.0, -1.0 / 3.0),
            (-(2.0 / 9.0f32).sqrt(), (2.0 / 3.0f32).sqrt(), -1.0 / 3.0),
            (-(2.0 / 9.0f32).sqrt(), -(2.0 / 3.0f32).sqrt(), -1.0 / 3.0),
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
    pub fn generate_vertex_buffer(&self) -> VertexBuffer {
        VertexBuffer(
            self.faces
                .iter_keys()
                .map(|face_key| {
                    self.find_cycle(&FaceKey(face_key))
                        .into_iter()
                        .filter_map(|edge_key| {
                            self.edges
                                .get(&edge_key.0)
                                .and_then(|edge| edge.origin)
                                .and_then(|origin_key| self.vertices.get(&origin_key.0))
                                .and_then(|origin| Some(origin.position))
                        })
                })
                .flatten()
                .collect(),
        )
    }

    pub fn find_cycle(&self, face_key: &FaceKey) -> Vec<HalfEdgeKey> {
        let mut result = Vec::new();
        if let Some(primary_edge_key) = self
            .faces
            .get(&face_key.0)
            .and_then(|face| face.primary_edge)
        {
            let mut edge_key = Some(primary_edge_key);
            while let Some(key) = edge_key {
                result.push(key);
                edge_key = match self.edges.get(&key.0).and_then(|edge| edge.next) {
                    Some(next_edge) if next_edge == primary_edge_key => None,
                    other => other,
                };
            }
        }
        result
    }

    pub fn find_outgoing_edges(&self, vertex: VertexKey) -> Vec<HalfEdgeKey> {
        todo!()
    }

    pub fn subdivide_edge(&mut self, edge_key: &HalfEdgeKey) -> HalfEdgeKey {
        // calculate the midpoint from the edge origin and the next origin
        let origin = self
            .edges
            .get(&edge_key.0)
            .and_then(|edge| edge.origin)
            .and_then(|vertex_key| self.vertices.get(&vertex_key.0))
            .expect("Edge has no corresponding Origin Point")
            .position;

        let endpoint = self
            .edges
            .get(&edge_key.0)
            .and_then(|edge| edge.next)
            .and_then(|next_key| self.edges.get(&next_key.0))
            .and_then(|next| next.origin)
            .and_then(|origin_key| self.vertices.get(&origin_key.0))
            .expect("Expected Valid origin for Edge's twin")
            .position;

        let midpoint_position = Point {
            x: 0.5 * (origin.x + endpoint.x),
            y: 0.5 * (origin.y + endpoint.y),
            z: 0.5 * (origin.z + endpoint.z),
        };

        // create a new point in our shape for the midpoint
        let bisect_point_key = VertexKey(self.vertices.insert(Vertex {
            position: midpoint_position,
            incident_edge: None,
        }));

        // create a new edge coming from the midpoint
        let bisected_edge_key = HalfEdgeKey(
            self.edges.insert(HalfEdge {
                origin: Some(bisect_point_key),
                prev: Some(*edge_key),
                ..self
                    .edges
                    .get(&edge_key.0)
                    .copied()
                    .expect("Could Not Dereferencee Edge")
            }),
        );

        // update our bisected edge to point to the new edge
        self.edges
            .get_mut(&edge_key.0)
            .expect("Could Not Dereference Edge")
            .next = Some(bisected_edge_key);
        // make edge.next.prev point to edge
        self.edges
            .get(&bisected_edge_key.0)
            .and_then(|edge| edge.next)
            .and_then(|next_key| self.edges.get_mut(&next_key.0))
            .expect("Could not Dereference Edge")
            .prev = Some(bisected_edge_key);
        self.vertices
            .get_mut(&bisect_point_key.0)
            .expect("Could Not Dereference Point")
            .incident_edge = Some(bisected_edge_key);

        // if our edge has a twin we need to update that twin too
        if let Some(twin_key) = self.edges.get(&edge_key.0).and_then(|edge| edge.twin) {
            let bisected_twin_key = HalfEdgeKey(
                self.edges.insert(HalfEdge {
                    origin: Some(bisect_point_key),
                    prev: Some(twin_key),
                    ..self
                        .edges
                        .get(&twin_key.0)
                        .copied()
                        .expect("Could not Dereference Edge")
                }),
            );

            // point twin.twin to the bisected (non-twin) edge
            self.edges
                .get_mut(&twin_key.0)
                .expect("could not dereference Edge")
                .twin = Some(bisected_edge_key);

            // point twin.next.prev to twin
            self.edges
                .get(&bisected_twin_key.0)
                .and_then(|edge| edge.next)
                .and_then(|next_key| self.edges.get_mut(&next_key.0))
                .expect("Could not Dereference Edge")
                .prev = Some(bisected_twin_key);

            // update the twin of our original edge
            self.edges
                .get_mut(&edge_key.0)
                .expect("Could not Dereference Edge")
                .twin = Some(bisected_twin_key);

            // point the old twin key to this new twin edge
            self.edges
                .get_mut(&twin_key.0)
                .expect("Could not dereference edge")
                .next = Some(bisected_twin_key);
        }

        bisected_edge_key
    }
}

fn loop_subdivision(shape: &DCEL) -> DCEL {
    let mut shape = shape.clone(); // clone the original shape

    // for ever edge insert a bisecting point, and create a new edge
    let original_edges: Vec<HalfEdgeKey> = shape
        .edges
        .iter_keys()
        .map(|raw_key| HalfEdgeKey(raw_key))
        .collect();

    let mut already_bisected_edges: HashSet<HalfEdgeKey> = HashSet::new();
    let new_edge_keys: Vec<HalfEdgeKey> = original_edges
        .into_iter()
        .filter_map(|edge_key| {
            if !already_bisected_edges.contains(&edge_key) {
                let bisected_edge_key = shape.subdivide_edge(&edge_key); // bisect the edge
                already_bisected_edges.insert(edge_key);
                // if that edge has a twin log it as being bisected too
                if let Some(bisected_twin_key) = shape
                    .edges
                    .get(&bisected_edge_key.0)
                    .and_then(|edge| edge.twin)
                {
                    already_bisected_edges.insert(bisected_twin_key);
                }
                Some(bisected_edge_key)
            } else {
                None
            }
        })
        .collect();

    // now we're going to divide each six-edge face into two four-edge faces
    //TODO: Finish this!
    let original_face_keys: Vec<FaceKey> = shape
        .faces
        .iter_keys()
        .map(|raw_key| FaceKey(raw_key))
        .collect();
    for face_key in original_face_keys {
        let primary_edge = *shape
            .faces
            .get(&face_key.0)
            .and_then(|face| face.primary_edge)
            .and_then(|edge_key| shape.edges.get(&edge_key.0))
            .expect("Could not dereference");

        let mut bisecting_edge = HalfEdge {
            origin: primary_edge.origin,
            twin: None,
            incident_face: None,
            next: shape
                .faces
                .get(&face_key.0)
                .and_then(|face| face.primary_edge),
            prev: None,
        };
        let mut bistecting_edge_twin = HalfEdge {
            origin: todo!(),
            twin: todo!(),
            incident_face: todo!(),
            next: todo!(),
            prev: todo!(),
        };
        // find our bisecting edge origin
        // create two half edges
        // create a new faces (one for each side of the bisecting edge
        // stitch the half-edge into the existing face
        //  update the face
    }

    // now starting at the new edge (A), advance once (B) and then insert an edge
    // from b.head to a.tail this will create our new subfaces
    let original_face_keys: Vec<FaceKey> = shape
        .faces
        .iter_keys()
        .map(|raw_key| FaceKey(raw_key))
        .collect();
    for face_key in original_face_keys {
        // the primary edge for the face is one of our original edges - if we collect that and take every two after we have the
        // edges that originate at an original vertex
        let original_edge_keys: Vec<HalfEdgeKey> =
            shape.find_cycle(&face_key).into_iter().step_by(2).collect();

        // for each original edge key we want to create a new face and an edge and twin to enclose the face
        // We have to keep track of the twins so we can create a face around them in the end
        let mut twin_keys = Vec::new();
        for edge_key in original_edge_keys {
            let edge_end_vertex_key = shape
                .edges
                .get(&edge_key.0)
                .and_then(|edge| edge.next)
                .and_then(|next_edge_key| shape.edges.get(&next_edge_key.0))
                .and_then(|next_edge| next_edge.origin)
                .expect("Expected Edge to have a next Edge");

            let prev_edge_key = shape
                .edges
                .get(&edge_key.0)
                .and_then(|edge| edge.prev)
                .expect("Expected Edge to have a Previous Edge");

            let prev_edge_origin_key = shape
                .edges
                .get(&prev_edge_key.0)
                .and_then(|edge| edge.origin)
                .expect("Expected Edge to have an Origin");

            // make our two new edges
            let enclosing_edge_key = HalfEdgeKey(shape.edges.insert(HalfEdge {
                origin: Some(edge_end_vertex_key),
                twin: None,
                incident_face: None,
                next: Some(prev_edge_key),
                prev: Some(edge_key),
            }));

            let enclosing_edge_twin_key = HalfEdgeKey(shape.edges.insert(HalfEdge {
                origin: Some(prev_edge_origin_key),
                twin: Some(enclosing_edge_key),
                incident_face: None,
                next: None,
                prev: None,
            }));

            twin_keys.push(enclosing_edge_twin_key); // push back the twin key

            // update twin of the enclosing edge
            shape
                .edges
                .get_mut(&enclosing_edge_key.0)
                .expect("expected Key to exist in shape")
                .twin = Some(enclosing_edge_twin_key);

            // point our current edge to the enclosing edge
            shape
                .edges
                .get_mut(&edge_key.0)
                .expect("Expected Key to Exist in Shape")
                .next = Some(enclosing_edge_key);

            // point the previous of our previous edge to the enclosing edge
            shape
                .edges
                .get_mut(&prev_edge_key.0)
                .expect("Expected key")
                .prev = Some(enclosing_edge_key);

            // make a new face and point all three of our edges to it
            let new_face_key = FaceKey(shape.faces.insert(Face {
                primary_edge: Some(edge_key),
            }));

            for key in [prev_edge_key, edge_key, enclosing_edge_key] {
                shape
                    .edges
                    .get_mut(&key.0)
                    .expect("Expected Key to Exist in Shape")
                    .incident_face = Some(new_face_key);
                //println!("{:?}", shape.edges.get(&key.0));
            }
        }
        // then we need to make a face from the inner region
        twin_keys
            .iter()
            .fold(Option::<&HalfEdgeKey>::None, |previous_key, key| {
                // if the previous key was not None, point it to the current key
                if let Some(previous_key) = previous_key {
                    shape
                        .edges
                        .get_mut(&previous_key.0)
                        .expect("Expected Key to Exist in Shape")
                        .next = Some(*key);
                }
                shape
                    .edges
                    .get_mut(&key.0)
                    .expect("Expected Key to Exist in Shape")
                    .prev = previous_key.copied();
                shape
                    .edges
                    .get_mut(&key.0)
                    .expect("Expected Key to Exist in Shape")
                    .incident_face = Some(face_key);
                Some(key)
            });
        shape
            .edges
            .get_mut(&twin_keys[0].0)
            .expect("Expected Key to exist in Shape")
            .prev = Some(twin_keys[2]);
        shape
            .edges
            .get_mut(&twin_keys[2].0)
            .expect("Expected Key to exist in Shape")
            .next = Some(twin_keys[0]);

        // remap the incident edge of the original face
        shape
            .faces
            .get_mut(&face_key.0)
            .expect("Expected Key to Exist in Shape")
            .primary_edge = Some(twin_keys[0]);
    }

    shape
}

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
}

#[wasm_bindgen]
pub fn greet() {
    alert("Hello, wasm-game-of-life!");
}

#[wasm_bindgen]
pub struct VertexBuffer(Vec<Point>);

#[wasm_bindgen]
impl VertexBuffer {
    pub fn new(iterations: u32) -> Self {
        (0..iterations)
            .fold(DCEL::tetrahedron(), |prev, _| loop_subdivision(&prev))
            .generate_vertex_buffer()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn buffer(&self) -> *const Point {
        self.0.as_ptr()
    }
}

#[cfg(test)]

mod tests {
    use super::*;
    use std::collections::HashSet;
    #[test]
    fn test_tetrahedron_vertices() {
        let shape = DCEL::tetrahedron();
        validate_mesh(&shape);

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
                shape
                    .edges
                    .get(&prev_key)
                    .and_then(|prev_edge| prev_edge.next)
                    .unwrap()
                    .0
            });
            let third_key_moving_backwards = (0..3).fold(edge_key, |prev_key, _| {
                shape
                    .edges
                    .get(&prev_key)
                    .and_then(|prev_edge| prev_edge.prev)
                    .unwrap()
                    .0
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
    #[test]
    fn test_loop_subdivision() {
        let shape = DCEL::tetrahedron();
        let subd = loop_subdivision(&shape);
        // we expect to now have 16 faces, 15 points and 48 edges
        assert_eq!(16, subd.faces.len(), "Incorrect Number of Faces");
        assert_eq!(10, subd.vertices.len(), "Incorrect Number of Vertices");
        assert_eq!(48, subd.edges.len(), "Incorrect Number of Edges");

        validate_mesh(&subd);
    }

    fn validate_mesh(shape: &DCEL) {
        for edge_key in shape.edges.iter_keys().map(|raw_key| HalfEdgeKey(raw_key)) {
            // the twin of a twin should be itself
            let twin_of_twin_key = shape
                .edges
                .get(&edge_key.0)
                .and_then(|edge| edge.twin)
                .and_then(|twin_key| shape.edges.get(&twin_key.0))
                .and_then(|twin| twin.twin)
                .unwrap();
            assert_eq!(twin_of_twin_key, edge_key);

            // the next of the prevoius and prev of next should be itself
            let previous_of_next_key = shape
                .edges
                .get(&edge_key.0)
                .and_then(|edge| edge.next)
                .and_then(|next_key| shape.edges.get(&next_key.0))
                .and_then(|next| next.prev)
                .unwrap();
            let next_of_previous_key = shape
                .edges
                .get(&edge_key.0)
                .and_then(|edge| edge.prev)
                .and_then(|prev_key| shape.edges.get(&prev_key.0))
                .and_then(|prev| prev.next)
                .unwrap();
            assert_eq!(previous_of_next_key, edge_key);
            assert_eq!(next_of_previous_key, edge_key);

            // the edge.origin and edge.twin.next.origin should be the same vertice
            let edge_origin_key = shape
                .edges
                .get(&edge_key.0)
                .and_then(|edge| edge.origin)
                .unwrap();
            let twin_next_origin_key = shape
                .edges
                .get(&edge_key.0)
                .and_then(|edge| edge.twin)
                .and_then(|twin_key| shape.edges.get(&twin_key.0))
                .and_then(|twin| twin.next)
                .and_then(|next_key| shape.edges.get(&next_key.0))
                .and_then(|next| next.origin)
                .unwrap();
            assert_eq!(
                edge_origin_key,
                twin_next_origin_key,
                "edge.origin is at {:?}, but twin.next.origin is {:?}",
                shape.vertices.get(&edge_origin_key.0).unwrap().position,
                shape
                    .vertices
                    .get(&twin_next_origin_key.0)
                    .unwrap()
                    .position
            );

            // edge.twin.origin and edge.next.origin should be the same vertice
            let edge_twin_origin_key = shape
                .edges
                .get(&edge_key.0)
                .and_then(|edge| edge.twin)
                .and_then(|twin_key| shape.edges.get(&twin_key.0))
                .and_then(|twin| twin.origin)
                .unwrap();
            let edge_next_origin_key = shape
                .edges
                .get(&edge_key.0)
                .and_then(|edge| edge.next)
                .and_then(|next_key| shape.edges.get(&next_key.0))
                .and_then(|next| next.origin)
                .unwrap();
            assert_eq!(edge_twin_origin_key, edge_next_origin_key);
        }

        // every edge in a face cycle should point to that face
        for face_key in shape.faces.iter_keys().map(|raw_key| FaceKey(raw_key)) {
            let cycle = shape.find_cycle(&face_key);
            assert_eq!(3, cycle.len(), "Surface Cycle has wrong number of edges");
            let all_faces_match = cycle
                .iter()
                .map(|edge_key| {
                    shape
                        .edges
                        .get(&edge_key.0)
                        .and_then(|edge| edge.incident_face)
                        .unwrap()
                })
                .fold(true, |all_same, cur| all_same && (cur == face_key));
            assert!(
                all_faces_match,
                "All edges for face {:?} do not associate with that face",
                face_key
            );
        }
    }
}
