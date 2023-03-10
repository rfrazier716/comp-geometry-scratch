import * as wasm from "dcel-test";
import { memory } from "dcel-test/comp_geometry_scratch_space_bg";
import * as THREE from "three";
import { OrbitControls } from "three/examples/jsm/controls/OrbitControls.js";
import Stats from "stats.js";

// add an fps counter
const stats = new Stats();
stats.showPanel(0); // 0: fps, 1: ms, 2: mb, 3+: custom
document.body.appendChild(stats.dom);
stats.dom.style.cssText = "position:absolute;top:100px;left:30px;";

// initialize our scene
const scene = new THREE.Scene();
const renderer = new THREE.WebGLRenderer();
renderer.setSize(0.75 * window.innerWidth, 0.75 * window.innerHeight);
document.body.appendChild(renderer.domElement);

// Camera and controls
const h_width = 5;
const aspect = window.innerWidth / window.innerWidth;
const camera = new THREE.PerspectiveCamera(
  undefined,
  window.innerWidth / window.innerHeight,
  0.1,
  100
);
const controls = new OrbitControls(camera, renderer.domElement);

camera.position.z = 5;

// generate our mesh
let shape = wasm.Cube.new();
let geometry = new THREE.BufferGeometry();
geometry.setAttribute(
  "position",
  new THREE.BufferAttribute(
    new Float32Array(
      memory.buffer,
      shape.get_buffer_start(),
      shape.get_buffer_length()
    ),
    3
  )
);

// Create a renderable counterpart
const material = new THREE.MeshBasicMaterial({
  polygonOffset: true,
  polygonOffsetFactor: 1,
  color: 0x00ff00,
  wireframe: true,
  side: THREE.DoubleSide,
});

let cube = new THREE.Mesh(geometry, material);

const incrementHandler = () => {
  console.log("starting subdivision");

  const start = performance.now();
  shape.subdivide(); // subdivide our shape
  const end = performance.now();
  console.log("subdivision ended: ", end - start);
  loadShape();
};

const resetHandler = () => {
  shape.free();
  shape = wasm.Cube.new();
  loadShape();
};

const loadShape = () => {
  document
    .getElementById("buffer-size")
    .replaceChildren(
      shape.get_buffer_length() / 9 +
        " faces.\t" +
        (shape.get_buffer_length() * 4) / 1000 +
        "kB buffer size"
    );
  let geometry = new THREE.BufferGeometry();
  geometry.setAttribute(
    "position",
    new THREE.BufferAttribute(
      new Float32Array(
        memory.buffer,
        shape.get_buffer_start(),
        shape.get_buffer_length()
      ),
      3
    )
  );

  cube.geometry.dispose();
  scene.remove(cube);
  cube = new THREE.Mesh(geometry, material);
  scene.add(cube);
};

document.getElementById("inc").addEventListener("click", incrementHandler);
document.getElementById("rst").addEventListener("click", resetHandler);

scene.add(cube);
const dx = Math.PI / 6.0;
const dy = -Math.PI / 6.0;

function animate() {
  stats.begin();
  requestAnimationFrame(animate);
  controls.update();
  cube.rotateX(0.005);
  cube.rotateY(0.0055);
  renderer.render(scene, camera);
  stats.end();
}
animate();
