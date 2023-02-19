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

    pub fn triangle() -> Self {
        let mut dcel = Self::new();

        let face_key = FaceKey(dcel.faces.insert(Face { primary_edge: None }));

        let point_keys: Vec<VertexKey> = [(-1.0, 0.0, 0.0), (1.0, 0.0, 0.0), (0.0, 1.0, 0.0)]
            .into_iter()
            .map(|(x, y, z)| {
                VertexKey(dcel.vertices.insert(Vertex {
                    position: Point::new(x, y, z),
                    incident_edge: None,
                }))
            })
            .collect();

        let final_edge_key = point_keys
            .into_iter()
            .fold(
                Option::<HalfEdgeKey>::None,
                |prev_edge_key, vertex_point_key| {
                    let new_edge_key = HalfEdgeKey(dcel.edges.insert(HalfEdge {
                        origin: Some(vertex_point_key),
                        twin: None,
                        incident_face: Some(face_key),
                        next: None,
                        prev: prev_edge_key,
                    }));
                    if let Some(prev_key) = prev_edge_key {
                        dcel.edges.get_mut(&prev_key.0).unwrap().next = Some(new_edge_key);
                    }
                    Some(new_edge_key)
                },
            )
            .unwrap();

        // link the first and last edges together
        let initial_edge = dcel
            .edges
            .get(&final_edge_key.0)
            .and_then(|edge| edge.prev)
            .and_then(|prev_key| dcel.edges.get(&prev_key.0))
            .and_then(|prev| prev.prev)
            .unwrap();

        dcel.edges.get_mut(&initial_edge.0).unwrap().prev = Some(final_edge_key);
        dcel.edges.get_mut(&final_edge_key.0).unwrap().next = Some(initial_edge);

        dcel.faces.get_mut(&face_key.0).unwrap().primary_edge = Some(initial_edge);

        dcel
    }

    pub fn from_vertices(vertices: &[Point], faces: &[(usize, usize, usize)]) -> Self {
        let mut dcel = DCEL::new();

        // insert the vertices into the shape
        let point_keys: Vec<VertexKey> = vertices
            .iter()
            .map(|vertex| {
                VertexKey(dcel.vertices.insert(Vertex {
                    position: *vertex,
                    incident_edge: None,
                }))
            })
            .collect();

        // create a hashmap to keep track of which half-edges already exist
        let mut created_edges: HashMap<(usize, usize), HalfEdgeKey> = HashMap::new();

        for (p1, p2, p3) in faces {
            // Create a new face in our DCEL
            let face = FaceKey(dcel.faces.insert(Face { primary_edge: None }));

            let edges_to_add = [(*p1, *p2), (*p2, *p3), (*p3, *p1)];
            // if we've seen any edges from point A->B already, we need to flip the orientation of the triangle
            // otherwise we'll have duplicate half-edges
            let requires_flip = edges_to_add
                .iter()
                .fold(false, |prev, cur| prev || created_edges.contains_key(cur));

            //Create our half edges and keep the keys so we can reference them later
            let edge_keys: Vec<HalfEdgeKey> = edges_to_add
                .into_iter()
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

    pub fn tetrahedron() -> Self {
        // DCEL for a tetrahedron Centered at the Origin
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

        let original_edge_twin_key = self.edges.get(&edge_key.0).and_then(|edge| edge.twin);
        let edges_to_split = if original_edge_twin_key.is_some() {
            vec![*edge_key, original_edge_twin_key.unwrap()]
        } else {
            vec![*edge_key]
        };

        let mut bisecting_edge_keys: Vec<HalfEdgeKey> = Vec::new();
        for this_edge in edges_to_split {
            // create the bisected edge from the midpoint
            let bisected_edge_key = HalfEdgeKey(
                self.edges.insert(HalfEdge {
                    origin: Some(bisect_point_key),
                    prev: Some(this_edge),
                    ..self
                        .edges
                        .get(&this_edge.0)
                        .copied()
                        .expect("Could Not Dereferencee Edge")
                }),
            );
            bisecting_edge_keys.push(bisected_edge_key); // push this into our array so we can reference it later

            // link it into the mesh
            self.edges
                .get(&bisected_edge_key.0)
                .and_then(|edge| edge.prev)
                .and_then(|prev_key| self.edges.get_mut(&prev_key.0))
                .unwrap()
                .next = Some(bisected_edge_key);
            self.edges
                .get(&bisected_edge_key.0)
                .and_then(|edge| edge.next)
                .and_then(|next_key| self.edges.get_mut(&next_key.0))
                .unwrap()
                .prev = Some(bisected_edge_key);

            // now we need to create a new face that so that we still only have three edges per face
            let rhs_face_key = FaceKey(self.faces.insert(Face {
                primary_edge: Some(bisected_edge_key),
            }));

            // update our new edge and the one following to point to this new face
            // by using fold we also get the origin point of our split edge
            let split_point_outgoing_edge_key = (0..2)
                .fold(Some(bisected_edge_key), |prev, _| {
                    if let Some(edge_key) = prev {
                        // update the face
                        self.edges
                            .get_mut(&edge_key.0)
                            .expect("expected Edge")
                            .incident_face = Some(rhs_face_key);
                        // point to the next edge
                        self.edges.get(&edge_key.0).and_then(|edge| edge.next)
                    } else {
                        None
                    }
                })
                .expect("Expected Iterator to Succeed");

            // create two new edges which split the face
            let split_edge_key = HalfEdgeKey(self.edges.insert(HalfEdge {
                twin: None,
                incident_face: Some(rhs_face_key),
                next: Some(bisected_edge_key),
                ..*self.edges.get(&split_point_outgoing_edge_key.0).unwrap()
            }));

            let split_edge_twin_key = HalfEdgeKey(
                self.edges.insert(HalfEdge {
                    origin: Some(bisect_point_key),
                    twin: Some(split_edge_key),
                    incident_face: self
                        .edges
                        .get(&this_edge.0)
                        .and_then(|edge| edge.incident_face),
                    next: Some(split_point_outgoing_edge_key),
                    prev: Some(this_edge),
                }),
            );

            // link the split edge key into the mesh
            self.edges.get_mut(&split_edge_key.0).unwrap().twin = Some(split_edge_twin_key);
            self.edges
                .get(&split_edge_key.0)
                .and_then(|edge| edge.next)
                .and_then(|next_key| self.edges.get_mut(&next_key.0))
                .unwrap()
                .prev = Some(split_edge_key);
            self.edges
                .get(&split_edge_key.0)
                .and_then(|edge| edge.prev)
                .and_then(|prev_key| self.edges.get_mut(&prev_key.0))
                .unwrap()
                .next = Some(split_edge_key);

            // Link the twin into the mesh
            self.edges
                .get(&split_edge_twin_key.0)
                .and_then(|edge| edge.next)
                .and_then(|next_key| self.edges.get_mut(&next_key.0))
                .unwrap()
                .prev = Some(split_edge_twin_key);
            self.edges
                .get(&split_edge_twin_key.0)
                .and_then(|edge| edge.prev)
                .and_then(|prev_key| self.edges.get_mut(&prev_key.0))
                .unwrap()
                .next = Some(split_edge_twin_key);
        }

        // lastly if our edge has a twin, we need to fix those references
        if let Some(twin_key) = original_edge_twin_key {
            self.edges.get_mut(&edge_key.0).unwrap().twin = bisecting_edge_keys.get(1).copied();
            self.edges.get_mut(&twin_key.0).unwrap().twin = bisecting_edge_keys.get(0).copied();
        }

        bisecting_edge_keys[0]
    }

    pub fn flip_edge(&mut self, edge_key: &HalfEdgeKey) -> Result<HalfEdgeKey, String> {
        // find your two anchors - which are edge.next and twin.next
        let twin_key = self
            .edges
            .get(&edge_key.0)
            .and_then(|edge| edge.twin)
            .ok_or(String::from("No Twin"))?;

        let mut anchors = [edge_key, &twin_key]
            .into_iter()
            .map(|key| self.edges.get(&key.0).and_then(|edge| edge.next).unwrap());
        let edge_next_key = anchors.next().unwrap();
        let twin_next_key = anchors.next().unwrap();

        // stitch the corner edges so you have a diamond and disconnect the edges to be flipped

        // anch_edge.prev = anch_twin.next
        // anch_twin.next.next = anch_edge
        let twin_prev_key = self
            .edges
            .get(&twin_next_key.0)
            .and_then(|twin| twin.next)
            .ok_or(String::from("index Error"))?;
        self.edges.get_mut(&edge_next_key.0).unwrap().prev = Some(twin_prev_key);
        self.edges.get_mut(&twin_prev_key.0).unwrap().next = Some(edge_next_key);

        let edge_prev_key = self
            .edges
            .get(&edge_next_key.0)
            .and_then(|edge| edge.next)
            .ok_or(String::from("index Error"))?;
        self.edges.get_mut(&twin_next_key.0).unwrap().prev = Some(edge_prev_key);
        self.edges.get_mut(&edge_prev_key.0).unwrap().next = Some(twin_next_key);

        // at this point we've removed the edges to flip from our cycles and can free them
        let to_flip_edge = self.edges.free(&edge_key.0)?;
        let to_flip_twin = self.edges.free(&twin_key.0)?;

        // make two new edges and insert them into the shape
        let flipped_edge_key = HalfEdgeKey(
            self.edges.insert(HalfEdge {
                origin: self
                    .edges
                    .get(&twin_prev_key.0)
                    .and_then(|edge| edge.origin),
                twin: None,
                next: Some(edge_prev_key),
                prev: Some(twin_next_key),
                ..to_flip_edge
            }),
        );

        let flipped_twin_key = HalfEdgeKey(
            self.edges.insert(HalfEdge {
                origin: self
                    .edges
                    .get(&edge_prev_key.0)
                    .and_then(|edge| edge.origin),
                twin: Some(flipped_edge_key),
                next: Some(twin_prev_key),
                prev: Some(edge_next_key),
                ..to_flip_twin
            }),
        );

        // close the twin cycle
        self.edges.get_mut(&flipped_edge_key.0).unwrap().twin = Some(flipped_twin_key);

        // Restitch our cycles together
        self.edges.get_mut(&twin_next_key.0).unwrap().next = Some(flipped_edge_key);
        self.edges.get_mut(&edge_prev_key.0).unwrap().prev = Some(flipped_edge_key);

        self.edges.get_mut(&edge_next_key.0).unwrap().next = Some(flipped_twin_key);
        self.edges.get_mut(&twin_prev_key.0).unwrap().prev = Some(flipped_twin_key);

        // fix the face for edges that have been modified
        self.edges.get_mut(&edge_next_key.0).unwrap().incident_face = self
            .edges
            .get(&flipped_twin_key.0)
            .and_then(|edge| edge.incident_face);
        self.edges.get_mut(&twin_next_key.0).unwrap().incident_face = self
            .edges
            .get(&flipped_edge_key.0)
            .and_then(|edge| edge.incident_face);

        // also since we deleted two edges we need to make sure their incident vertices and faces don't accidentally point to them
        self.edges
            .get(&edge_next_key.0)
            .and_then(|edge| edge.origin)
            .and_then(|origin_key| self.vertices.get_mut(&origin_key.0))
            .unwrap()
            .incident_edge = Some(edge_next_key);
        self.edges
            .get(&edge_next_key.0)
            .and_then(|edge| edge.incident_face)
            .and_then(|face_key| self.faces.get_mut(&face_key.0))
            .unwrap()
            .primary_edge = Some(edge_next_key);

        self.edges
            .get(&twin_next_key.0)
            .and_then(|edge| edge.origin)
            .and_then(|origin_key| self.vertices.get_mut(&origin_key.0))
            .unwrap()
            .incident_edge = Some(twin_next_key);
        self.edges
            .get(&twin_next_key.0)
            .and_then(|edge| edge.incident_face)
            .and_then(|face_key| self.faces.get_mut(&face_key.0))
            .unwrap()
            .primary_edge = Some(twin_next_key);

        Ok(flipped_edge_key)
    }
}

fn loop_subdivision(shape: &DCEL) -> DCEL {
    let mut shape = shape.clone(); // clone the original shape

    let original_vertices: HashSet<VertexKey> =
        HashSet::from_iter(shape.vertices.iter_keys().map(|raw_key| VertexKey(raw_key)));

    // Subdivide every edge in the mesh and collect the ones that may need to be flipped
    let mut subdivided_edges = HashSet::new();
    let edges_to_split: Vec<HalfEdgeKey> = shape
        .edges
        .iter_keys()
        .map(|raw_key| HalfEdgeKey(raw_key))
        .collect();

    let bisecting_edges: Vec<HalfEdgeKey> = edges_to_split
        .into_iter()
        .filter_map(|edge_key| {
            if !subdivided_edges.contains(&edge_key) {
                // put this edge and it's twin into the set
                subdivided_edges.insert(edge_key);
                if let Some(twin_key) = shape.edges.get(&edge_key.0).and_then(|edge| edge.twin) {
                    subdivided_edges.insert(twin_key);
                }
                // subdivide the edge
                let new_edge_key = shape.subdivide_edge(&edge_key);
                // get the primary and twin bisecting edges
                let primary_bisecting_edge_key = shape
                            .edges
                            .get(&new_edge_key.0)
                            .and_then(|edge| edge.prev);
                let twin_bisecting_edge_key = shape
                                .edges
                                .get(&new_edge_key.0)
                                .and_then(|edge| edge.twin)
                                .and_then(|twin_key| shape.edges.get(&twin_key.0))
                                .and_then(|twin| twin.next)
                                .and_then(|next_key| shape.edges.get(&next_key.0))
                                .and_then(|next| next.twin);
                Some([primary_bisecting_edge_key, twin_bisecting_edge_key].into_iter())

            } else {
                None
            }
        }).flatten().filter_map(|edge| edge).collect();

    // go through all the new edges
    // if the bisecting edge origin is in our original vertices, flip it!
    for edge_key in bisecting_edges {
        if original_vertices.contains(&shape.edges.get(&edge_key.0).and_then(|edge| edge.origin).unwrap()){
            shape.flip_edge(&edge_key).unwrap();
        }
    }

    //         // we need to investigate if we'll need to flip the bisecting edges, which are edge.prev and edge.twin.next.twin
    //         // if either of those originate on a vertice that is "original", e.g. was not created based on a subdivision, they need to be subdivided
    //         let primary_bisecting_edge_key = shape
    //             .edges
    //             .get(&new_edge_key.0)
    //             .and_then(|edge| edge.prev)
    //             .unwrap();
    //         let edges_to_investigate = if shape
    //             .edges
    //             .get(&new_edge_key.0)
    //             .and_then(|edge| edge.twin)
    //             .is_some()
    //         {
    //             let twin_bisecting_edge_key = shape
    //                 .edges
    //                 .get(&new_edge_key.0)
    //                 .and_then(|edge| edge.twin)
    //                 .and_then(|twin_key| shape.edges.get(&twin_key.0))
    //                 .and_then(|twin| twin.next)
    //                 .and_then(|next_key| shape.edges.get(&next_key.0))
    //                 .and_then(|next| next.twin)
    //                 .unwrap();

    //             vec![primary_bisecting_edge_key, twin_bisecting_edge_key]
    //         } else {
    //             vec![primary_bisecting_edge_key]
    //         };
    //         Some(edges_to_investigate.into_iter())
    //     } else {
    //         None // if this edges twin was already divided, we'll filter it out
    //     }
    // })

    // then for every edge in the mesh if edge.origin or edge.next.origin is a vertice created by an edge bisection - flip it!
    // let mut flipped_edges = HashSet::new();
    // let edges_to_flip: Vec<HalfEdgeKey> = shape
    //     .edges
    //     .iter_keys()
    //     .map(|raw_key| HalfEdgeKey(raw_key))
    //     .collect();

    // for edge in edges_to_flip{
    //     if !
    // }

    return shape;
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

    #[test]
    fn test_edge_split() {
        let mut shape = DCEL::triangle();
        let edges: Vec<HalfEdgeKey> = shape
            .edges
            .iter_keys()
            .map(|raw_key| HalfEdgeKey(raw_key))
            .collect();
        let expected_results = vec![(2, 6, 4), (3, 9, 5), (4, 12, 6)];

        for (edge_key, (expected_faces, expected_edges, expected_vertices)) in
            edges.iter().zip(expected_results.into_iter())
        {
            shape.subdivide_edge(edge_key);
            assert_eq!(shape.faces.len(), expected_faces);
            assert_eq!(shape.edges.len(), expected_edges);
            assert_eq!(shape.vertices.len(), expected_vertices);

            for face_key in shape.faces.iter_keys().map(|raw_key| FaceKey(raw_key)) {
                assert_eq!(shape.find_cycle(&face_key).len(), 3);
            }
        }

        // now check if we split an edge with a twin
        let mut shape = DCEL::triangle();
        let new_half_edge = shape.subdivide_edge(&HalfEdgeKey(Key {
            index: 0,
            generation: 0,
        }));
        let edge_with_twin = shape
            .edges
            .get(&new_half_edge.0)
            .and_then(|edge| edge.prev)
            .unwrap();
        shape.subdivide_edge(&edge_with_twin);

        // we would expect 4 faces, 12 edges, but only 5 vertices
        assert_eq!(shape.faces.len(), 4);
        assert_eq!(shape.edges.len(), 12);
        assert_eq!(shape.vertices.len(), 5);

        for face_key in shape.faces.iter_keys().map(|raw_key| FaceKey(raw_key)) {
            assert_eq!(shape.find_cycle(&face_key).len(), 3);
        }
    }

    #[test]
    fn test_edge_flip() {
        // make a diamond with a twin in the middle
        let vertices: Vec<Point> = [
            (-1.0, 0.0, 0.0),
            (0.0, 1.0, 0.0),
            (1.0, 0.0, 0.0),
            (0.0, -1.0, 0.0),
        ]
        .into_iter()
        .map(|(x, y, z)| Point { x, y, z })
        .collect();

        let face_indices = vec![(0, 2, 1), (2, 0, 3)];

        let mut shape = DCEL::from_vertices(&vertices, &face_indices);

        let twinned_edges: Vec<HalfEdgeKey> = shape
            .edges
            .iter_keys()
            .map(|raw_key| HalfEdgeKey(raw_key))
            .filter(|edge_key| {
                shape
                    .edges
                    .get(&edge_key.0)
                    .and_then(|edge| edge.twin)
                    .is_some()
            })
            .collect(); // should have len()=2

        // flip one of the twinned edges
        shape.flip_edge(&twinned_edges[0]).unwrap();

        // The Mesh should remain valid
        validate_mesh(&shape);

        // we should still have 6 edges, 4 vertices, and 2 faces
        assert_eq!(shape.edges.len(), 6);
        assert_eq!(shape.vertices.len(), 4);
        assert_eq!(shape.faces.len(), 2);

        // the points making up the flipped edge should now be (0,1,3) and (2,3,1)
        // e.g the x coordinates of an edge should always be greater than zero or less than zero
        // quick way to test is to sum up the x coordinates and make sure they're nonzero
        for face in shape.faces.iter_keys().map(|raw_key| FaceKey(raw_key)) {
            let sum_of_x_coords = shape
                .find_cycle(&face)
                .iter()
                .map(|edge_key| {
                    shape
                        .edges
                        .get(&edge_key.0)
                        .and_then(|edge| edge.origin)
                        .and_then(|origin_key| shape.vertices.get(&origin_key.0))
                        .unwrap()
                        .position
                        .x
                })
                .fold(0.0, |sum, val| sum + val);
            assert!(sum_of_x_coords > 0.5 || sum_of_x_coords < -0.5);
        }
    }

    fn validate_mesh(shape: &DCEL) {
        for edge_key in shape.edges.iter_keys().map(|raw_key| HalfEdgeKey(raw_key)) {
            // These tests are only done on edges with twins
            if shape
                .edges
                .get(&edge_key.0)
                .and_then(|edge| edge.twin)
                .is_some()
            {
                let twin_of_twin_key = shape
                    .edges
                    .get(&edge_key.0)
                    .and_then(|edge| edge.twin)
                    .and_then(|twin_key| shape.edges.get(&twin_key.0))
                    .and_then(|twin| twin.twin)
                    .unwrap();

                assert_eq!(twin_of_twin_key, edge_key);

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
