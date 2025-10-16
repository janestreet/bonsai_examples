# Bonsai Examples

A collection of examples for [Bonsai](https://github.com/janestreet/bonsai), a library for building reusable UI components using Js_of_ocaml.

## Getting Started

### Docker Setup (Recommended for macOS). The following has only been tested for the /keyboard example

If you're on macOS and want a consistent development environment, you can use the provided Docker setup:

1. **Build the Docker image:**
   ```bash
   docker build -t bonsai-examples .
   ```

2. **Run the container with your source code mounted and port forwarding:**
   ```bash
   docker run -it --rm \
     -v $(pwd):/workspace \
     -p 3000:3000 \
     bonsai-examples
   ```

3. **Inside the container, build the keyboard example, copy the compiled js file into the folder with the index.html file, and then run a web server:**
   ```bash
   dune build keyboard/main.bc.js

   cd keyboard
   cp ../_build/default/keyboard/main.bc.js .

   python3 -m http.server 3000
   ```

   Note: The js file may take up to several minutes to load. You can track loading progress in the network tab of browser dev tools.

4. **Access the example in your browser:**
   - Open http://localhost:3000 in your web browser

### Tested Examples

- **keyboard/**: Keyboard input handling example
- *(Add other examples as they're available)*

### Development

The project uses:
- **Bonsai** for reactive UI components
- **Js_of_ocaml** for compiling OCaml to JavaScript
- **Virtual_dom** for DOM manipulation
- **Async** for asynchronous programming

### Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for contribution guidelines.

### License

See [LICENSE.md](LICENSE.md) for license information.