use crate::allocator::{Allocator, Key};

#[derive(Default)]
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

#[derive(Default)]
struct Vertex {
    position: Point,            // the 3D coordinate of the point
    incident_edge: Option<Key>, // Refers to a single edge incident to the point
}

#[derive(Default)]

struct HalfEdge {
    origin: Option<Key>,
    twin: Option<Key>,
    incident_face: Option<Key>,
    next: Option<Key>,
    prev: Option<Key>,
}

#[derive(Default)]
struct Face {
    primary_edge: Option<Key>,
}

struct DCEL {
    vertices: Allocator<Vertex>,
    edges: Allocator<Key>,
    faces: Allocator<Key>,
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
        let points: Vec<Key> = [
            ((8.0 / 9.0f64).sqrt(), 0.0, -1.0 / 3.0),
            (-(2.0 / 9.0f64).sqrt(), (2.0 / 3.0f64).sqrt(), -1.0 / 3.0),
            (-(2.0 / 9.0f64).sqrt(), -(2.0 / 3.0f64).sqrt(), -1.0 / 3.0),
            (0.0, 0.0, 1.0),
        ]
        .into_iter()
        .map(|(x, y, z)| {
            dcel.vertices.insert(Vertex {
                position: Point::new(x, y, z),
                incident_edge: None,
            })
        })
        .collect();

        // faces are references to the
        let faces = [[0, 1, 2], [2, 1, 3], [0, 2, 3], [0, 3, 1]].map(
            |vertex_indices| {
                let mut shifted = vertex_indices.clone();
                shifted.rotate_left(1); // rotate by one so we get segments
                vertex_indices.into_iter().zip(shifted.into_iter()).map(|(v1, v2)| {
                    // create our half edge going from v1 -> v2
                    
                })
            }
        );
        dcel
    }
}

impl DCEL {
    pub fn find_cycle(&self, initial_edge: Key) -> Vec<Key> {
        // Returns a vector of edge keys that form a cycle around a face
        todo!()
    }

    pub fn find_outgoing_edges(&self, vertex: Key) -> Vec<Key> {
        todo!()
    }

    pub fn find_incoming_edges(&self, vertex: Key) -> Vec<Key> {
        todo!()
    }
}
