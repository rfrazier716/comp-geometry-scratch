import * as wasm from "dcel-test";
import {memory} from "dcel-test/comp_geometry_scratch_space_bg"
import * as THREE from 'three';

const scene = new THREE.Scene();
const camera = new THREE.PerspectiveCamera( 75, window.innerWidth / window.innerHeight, 0.1, 1000 );

const renderer = new THREE.WebGLRenderer();
renderer.setSize( window.innerWidth, window.innerHeight );
document.body.appendChild( renderer.domElement );


camera.position.z = 5;


let tetrahedron = wasm.VertexBuffer.new();
const vertices = new Float32Array(memory.buffer, tetrahedron.buffer(), 576);
console.log(vertices)
console.log("Hello World")
const geometry = new THREE.BufferGeometry();
geometry.setAttribute( 'position', new THREE.BufferAttribute( vertices, 3 ) );
const material = new THREE.MeshBasicMaterial( { color: 0x00ff00,  wireframe: true} );

const cube = new THREE.Mesh( geometry, material );
scene.add( cube );


function animate() {
	requestAnimationFrame( animate );
    cube.rotateX(0.01);
    cube.rotateY(0.015);
	renderer.render( scene, camera );
}
animate();
