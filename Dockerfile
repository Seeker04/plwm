# MIT License, Copyright (c) 2023-2025 Barnabás Zahorán, see LICENSE

# Use the official Ubuntu base image
FROM ubuntu:latest

# Avoid interactive prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Use UTF-8 encoding, otherwise swipl throws "Illegal multibyte Sequence" errors
ENV LANG=en_US.UTF-8

# Update package list and install required packages
RUN apt-get update &&   \
    apt-get install -y  \
    sudo                \
    git                 \
    build-essential     \
    libx11-dev          \
    libxft-dev          \
    libxrandr-dev       \
    swi-prolog          \
    cppcheck            \
    clang-tidy          \
    && apt-get clean    \
    && rm -rf /var/lib/apt/lists/*

# Create testuser with nopasswd sudo privilege
RUN useradd -m -s /bin/bash testuser && \
    usermod -aG sudo testuser && \
    echo "testuser ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers

# Set the working directory
WORKDIR /home/testuser/workspace

# Copy project files into the container
COPY . .

# Ensure the workspace directory is owned by testuser
RUN chown -R testuser:testuser /home/testuser

# Switch to the testuser
USER testuser

# Run all checks
CMD ["bash", "-c", "time tests/check_all.sh"]
