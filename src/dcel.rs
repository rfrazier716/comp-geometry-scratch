use crate::allocator::{Allocator, Key};

#[derive(Default)]
struct VertexKey(Key);

#[derive(Default)]
struct HalfEdgeKey(Key);

#[derive(Default)]
struct FaceKey(Key);

#[derive(Default)]
struct Point {
    x: f64,
    y: f64,
    z: f64,
}

#[derive(Default)]
struct Vertex {
    position: Point,            // the 3D coordinate of the point
    incident_edge: Option<HalfEdgeKey>, // Refers to a single edge incident to the point
}

#[derive(Default)]

struct HalfEdge {
    origin: Option<VertexKey>,
    twin: Option<HalfEdgeKey>,
    incident_face: Option<FaceKey>,
    next: Option<HalfEdgeKey>,
    prev: Option<HalfEdgeKey>,
}

#[derive(Default)]
struct Face {
    primary_edge: Option<HalfEdgeKey>,
}

struct DCEL {
    vertices: Allocator<Vertex>,
    edges: Allocator<HalfEdge>,
    faces: Allocator<Face>,
}
