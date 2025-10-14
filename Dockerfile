FROM ocaml/opam:debian-12-ocaml-5.2

ENV OPAMYES=1 OPAMJOBS=4
USER root
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential m4 pkg-config git curl rsync ca-certificates \
    libgmp-dev libssl-dev zlib1g-dev libffi-dev \
 && rm -rf /var/lib/apt/lists/*
USER opam

SHELL ["/bin/bash", "-lc"]

# Repos: bleeding, bleeding-external, then default
RUN opam repo add janestreet-bleeding https://github.com/janestreet/opam-repository.git --all-switches --dont-select || true \
 && opam repo add janestreet-bleeding-external https://github.com/janestreet/opam-repository.git#external-packages --all-switches --dont-select || true \
 && opam repo add default https://opam.ocaml.org --all-switches --dont-select || true \
 && opam repo set-repos janestreet-bleeding janestreet-bleeding-external default \
 && opam update

# Install dependencies from project manifest
WORKDIR /workspace
COPY --chown=opam:opam dune-project bonsai_examples.opam ./
RUN opam install . --deps-only --with-test \
 && opam clean -a -c -s

# Copy source code
COPY --chown=opam:opam . .

CMD ["bash", "-lc", "eval $(opam env) && bash"]
