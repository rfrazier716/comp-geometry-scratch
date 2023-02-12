pub mod allocator;

use std::collections::HashMap;
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
                println!("{:?}", edge_key);
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
}

fn loop_subdivision(shape: &DCEL) -> DCEL {
    let mut shape = shape.clone(); // clone the original shape

    // for ever edge insert a bisecting point, and create a new edge
    let new_edges: Vec<HalfEdge> = shape
        .edges
        .iter_keys()
        .map(|raw_key| HalfEdgeKey(raw_key))
        .scan(
            HashMap::<HalfEdgeKey, VertexKey>::new(),
            |bisect_lut, edge_key| {
                let edge = shape
                    .edges
                    .get(&edge_key.0)
                    .expect("Edge Key does not Exist in Shape");
                // check if the edges twin is in the the HashMap
                let bisection_point_key =
                    match edge.twin.and_then(|twin_key| bisect_lut.get(&twin_key)) {
                        Some(bisect_key) => *bisect_key,
                        None => {
                            // calculate the mid-point of the edge
                            let origin = edge
                                .origin
                                .and_then(|vertex_key| shape.vertices.get(&vertex_key.0))
                                .expect("Edge has no corresponding Origin Point")
                                .position;
                            let endpoint = edge
                                .twin
                                .and_then(|twin_key| shape.edges.get(&twin_key.0))
                                .and_then(|twin| twin.origin)
                                .and_then(|origin_key| shape.vertices.get(&origin_key.0))
                                .expect("Expected Valid origin for Edge's twin")
                                .position;
                            // create a new point and insert it into the hashmap with this edge key as the key
                            let midpoint_position = Point {
                                x: 0.5 * (origin.x + endpoint.x),
                                y: 0.5 * (origin.y + endpoint.y),
                                z: 0.5 * (origin.z + endpoint.z),
                            };
                            VertexKey(shape.vertices.insert(Vertex {
                                position: midpoint_position,
                                incident_edge: None,
                            }))
                        }
                    };

                // make a new edge that goes from the bisection point to the next edge in the face
                // We can't insert it into the edges yet - Borrowing Rules?
                Some(HalfEdge {
                    origin: Some(bisection_point_key),
                    prev: Some(edge_key),
                    ..*edge
                })
            },
        )
        .collect();

    // insert the new edges into our shape;
    let new_edge_keys: Vec<HalfEdgeKey> = new_edges
        .into_iter()
        .map(|half_edge| HalfEdgeKey(shape.edges.insert(half_edge)))
        .collect();

    // stitch together the twins of the edges
    // the the next-edge of a twin to a newly created edge should also be a newly created edge. And it is the twin we should be updating the original edge with
    // For external edges, there won't be a twin (at least how it's currently defined)
    for edge_key in new_edge_keys {
        if let Some(next_of_twin_key) = shape
            .edges
            .get(&edge_key.0)
            .and_then(|edge| edge.twin)
            .and_then(|twin_key| shape.edges.get(&twin_key.0))
            .and_then(|twin| twin.next)
        {
            let prev_edge = shape
                .edges
                .get(&edge_key.0)
                .and_then(|edge| edge.prev)
                .and_then(|prev_key| shape.edges.get_mut(&prev_key.0))
                .expect("Expected Key to exist in shape");
            prev_edge.twin = Some(next_of_twin_key); // fix the previous edges twin
            prev_edge.next = Some(edge_key); // point the next of the previous edge
        }
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
    pub fn new() -> Self {
        DCEL::tetrahedron().generate_vertex_buffer()
    }

    pub fn buffer(&self) -> *const Point {
        self.0.as_ptr()
    }
}

#[cfg(test)]

mod tests {
    use super::*;
    use std::{collections::HashSet, os::linux::raw};
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
        for edge_key in subd.edges.iter_keys().map(|raw_key| HalfEdgeKey(raw_key)){
            println!("{:?} -> {:?}", edge_key, subd.edges.get(&edge_key.0).and_then(|edge| edge.next)); 
        }  
        println!("{:?}", subd.edges);
        for len in subd
            .faces
            .iter_keys()
            .map(|raw_key| FaceKey(raw_key))
            .map(|face_key| shape.find_cycle(&face_key))
            .map(|cycle| cycle.len())
        {
            println!("{:?}", len);
        }
        // println!("{:?}", subd.faces);
    }
}