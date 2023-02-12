#[derive(Debug, Copy, Clone, PartialEq)]
struct Point2D {
    x: f64,
    y: f64,
}

impl Point2D {
    fn new(x: f64, y: f64) -> Self {
        Point2D { x, y }
    }
}

#[derive(Debug, PartialEq)]
enum SegmentOrientation {
    Left,
    Right,
    Colinear,
}

fn orientation(point1: &Point2D, point2: &Point2D, point3: &Point2D) -> SegmentOrientation {
    // If we view the three points as A, B, and C, and Lines AB and BC,
    // If the z- component of the cross product of ABxBC is positive, it's left oriented
    // if it's negative it's right oriented
    let cross_product = (point2.x - point1.x) * (point3.y - point2.y)
        - (point2.y - point1.y) * (point3.x - point2.x);
    if cross_product > 0.0 {
        SegmentOrientation::Left
    } else if cross_product < 0.0 {
        SegmentOrientation::Right
    } else {
        SegmentOrientation::Colinear
    }
}

fn convex_hull(points: &Vec<Point2D>) -> Option<Vec<Point2D>> {
    if points.len() < 3 {
        return None;
    }

    // lexically sort our points from left to right, bottom to top
    let mut sorted_points = points.to_vec();
    sorted_points.sort_by(|a, b| match a.x.total_cmp(&b.x) {
        core::cmp::Ordering::Equal => a.y.total_cmp(&b.y),
        cmp => cmp,
    });
    println!("sorted: {:?}", sorted_points);
    let mut upper_hull = Vec::new();

    for point in &sorted_points {
        if upper_hull.len() < 2 {
            // don't have enough points yet, push back the buffer
            upper_hull.push(*point)
        } else {
            println!("point: {:?}", point);
            while orientation(&upper_hull[upper_hull.len()-2], &upper_hull.last().unwrap(), point) == SegmentOrientation::Left {
                upper_hull.pop(); // pop the offending element
            }
            upper_hull.push(*point);
        }
    }
    let upper_hull_points = upper_hull.len();
    for point in sorted_points.iter().rev().skip(1) {
        if upper_hull.len() < upper_hull_points + 1 {
            // don't have enough points yet, push back the buffer
            upper_hull.push(*point)
        } else {
            println!("point: {:?}", point);
            while orientation(&upper_hull[upper_hull.len()-2], &upper_hull.last().unwrap(), &point) == SegmentOrientation::Left {
                upper_hull.pop(); // pop the offending element
            }
            upper_hull.push(*point);
        }
    }
    Some(upper_hull)
}

fn main() {
    // let points = vec![
    //     Point2D::new(0.0, 3.0),
    //     Point2D::new(2.0, 3.0),
    //     Point2D::new(1.0, 2.0),
    //     Point2D::new(3.0, 2.0),
    //     Point2D::new(4.0, 2.0),
    //     Point2D::new(2.0, 1.0),
    //     Point2D::new(3.0, 1.0),
    //     Point2D::new(0.0, 0.0),
    //     Point2D::new(2.0, 0.0),
    //     Point2D::new(4.0, 0.0),
    //     Point2D::new(1.0, -1.0),
    //     Point2D::new(5.0, -1.0),
    //     Point2D::new(3.0, -2.0),
    // ];
    // println!("{:?}", convex_hull(&points));
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn test_orientation() {
        let test_table = vec![
            (
                [
                    Point2D::new(0.0, 0.0),
                    Point2D::new(1.0, 1.0),
                    Point2D::new(2.0, 2.0),
                ],
                SegmentOrientation::Colinear,
            ),
            (
                [
                    Point2D::new(0.0, 0.0),
                    Point2D::new(1.0, 1.0),
                    Point2D::new(2.0, 1.0),
                ],
                SegmentOrientation::Right,
            ),
            (
                [
                    Point2D::new(0.0, 0.0),
                    Point2D::new(1.0, 1.0),
                    Point2D::new(1.0, 2.0),
                ],
                SegmentOrientation::Left,
            ),
        ];
        for (points, expected) in test_table {
            assert_eq!(expected, orientation(&points[0], &points[1], &points[2]));
        }
    }
}
