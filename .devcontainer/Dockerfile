FROM ubuntu:22.04

# Avoid interactive tzdata, etc.
ENV DEBIAN_FRONTEND=noninteractive

# Install GNU COBOL and useful tools
RUN apt-get update &&         apt-get install -y --no-install-recommends             gnucobol             build-essential             gdb             make             git             locales             ca-certificates             && rm -rf /var/lib/apt/lists/*

# Configure UTF-8 locale
RUN locale-gen en_US.UTF-8
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

# Create workspace
WORKDIR /workspace

# Default command
CMD ["/bin/bash"]
