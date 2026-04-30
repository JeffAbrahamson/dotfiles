# Docker

This directory defines the containerized environment used to run development checks for the repository. It gives the project a repeatable way to run tests without depending on the host machine's exact package set.

## Contents

* [`Dockerfile`](Dockerfile) defines the base image.
* [`compose.yml`](compose.yml) describes the development/test container setup.
* [`docker-manage.sh`](docker-manage.sh) wraps common container lifecycle tasks so day-to-day use stays short and consistent.
* [`stuff/`](stuff/README.md) contains entrypoint and package-install helper scripts used by the image build.
