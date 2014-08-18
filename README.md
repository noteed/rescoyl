# The Rescoyl Docker registry

Rescoyl is a Docker registry. It is tested against the official
[docker-registry](https://github.com/docker/docker-registry) project using
[Rescoyl Checks](https://github.com/noteed/rescoyl-checks). It is currently in
development.

## Building the image

To create a local Docker image with Rescoyl ready to run, simply run:

    > make

Building the image requires `images.reesd.com/reesd/stack`, an image containing
GHC and some dependencies. For that reason, the simplest way to run the image
on any machine without registry is to dump the resulting image as a tarball.
The Makefile can creates such a tarball:

    > make rescoyl.tar.xz

On another machine, loading the tarball within Docker is self-sufficient:

    > docker load -i rescoyl.tar.xz

In both cases, you will end up with an image called `noteed/rescoyl`.

Note: that `images.reesd.com/reesd/stack` is not yet public. Until then, simply
`cabal install` and then optionally build the Docker image (see in the Makefile
for an example).

## Running

To run the image locally, e.g. for testing, there is no need to forward ports:

    > docker run -d noteed/rescoyl

Note: you need to make sure that `registry.local` (or whatever you want to call
it to test it) resolves to your running Docker container, possibly by editing
your `/etc/hosts` file.

Otherwise:

    > docker run -d -p 80:80 noteed/rescoyl

Note: normally the storage area of the container should be mapped on the host
file system, for instance:

    > mkdir /IMAGES
    > docker run -d -p 80:80 -v /IMAGES:/store noteed/rescoyl

## Testing

The tests run in their own container and they will download an ubuntu:12.04
image. To avoid re-downloading that image repeatedly, you can do it once and
keep around the resulting volume. The test script will use the volume from a
container named `dind-rescoyl`.

    > docker run --name dind-rescoyl \
      -t -i --privileged images.reesd.com/hypered/dind
    root@aaaaaaaaaaaa:/# docker pull ubuntu:12.04
    ^D

After you can check that running a container with `--volumes-from dind-rescoyl`
will have the ubuntu:12.04 image already present.

The tests are run as follow:

    > ./integration rescoyl

Note that you need a copy of the `http-checks` executable in the current
directory.
