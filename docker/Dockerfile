FROM ubuntu:24.04 AS compile-dev-base

RUN mkdir -p   /usr/local/

WORKDIR /devsrc

# These COPY commands ought not to invalidate cache unless the copied
# file has changed.
COPY --chmod=755 docker/stuff/packages-runtime.sh ./
RUN ./packages-runtime.sh 2>&1 | tee /var/packages-runtime.out

COPY --chmod=755 docker/stuff/packages-dev.sh ./
RUN ./packages-dev.sh 2>&1 | tee /var/packages-dev.out

RUN useradd -ms /bin/bash dev
RUN apt-get update && apt-get install -y --no-install-recommends \
      gosu \
    && rm -rf /var/lib/apt/lists/*
COPY --chmod=755 docker/stuff/entrypoint.sh /usr/local/bin/entrypoint.sh

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
CMD ["bash"]



FROM compile-dev-base AS compile-for-dev


FROM compile-dev-base AS compile-for-claude

RUN apt-get update && apt-get install -y nodejs npm

# Install Claude Code for AI-assisted development
RUN npm install -g @anthropic-ai/claude-code


FROM compile-dev-base AS compile-for-codex

RUN apt-get update && apt-get install -y nodejs npm

# Install OpenAI Codex CLI for AI-assisted development
RUN npm install -g @openai/codex
