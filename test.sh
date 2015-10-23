#! /bin/bash

die() {
  echo "Failure: $1 (exiting...)"
  exit 1
}

if [ ! -f /usr/share/ca-certificates/self-certificate.crt ] ; then
  # Add our self-signed certificate (Docker really wants HTTPS with a cert).
  cp /source/self-certificate.crt /usr/share/ca-certificates/self-certificate.crt
  echo self-certificate.crt >> /etc/ca-certificates.conf
  update-ca-certificates --fresh
fi

docker build -t registry.local/quux/bar - << EOF
FROM ubuntu:12.04
RUN echo Hello, world > /hello
CMD cat /hello
EOF

docker tag registry.local/quux/bar registry.local/quux/bar:alpha
docker tag registry.local/quux/bar registry.local/quux/bar:beta

# Pre-create the .dockercfg file so that hitting the registry doens't cause the
# docker client to ask for a username and a password
echo -n '{"registry.local":{"auth":"' > /root/.dockercfg
echo -n 'quux:thud' | openssl enc -base64 -A >> /root/.dockercfg
echo '","email":"quux@example.com"}}' >> /root/.dockercfg

docker push registry.local/quux/bar
docker rmi registry.local/quux/bar
docker rmi registry.local/quux/bar:alpha
docker rmi registry.local/quux/bar:beta
docker run --rm registry.local/quux/bar | grep -q "Hello, world" || die "Expected 'Hello, world'"
docker run --rm registry.local/quux/bar:beta | grep -q "Hello, world" || die "Expected 'Hello, world'"

docker push registry.local/quux/bar

docker rmi registry.local/quux/bar
# registry.local/quux/bar:alpha doens't exist locally
docker rmi registry.local/quux/bar:beta

# Try to pull a "public" image.
mv /root/.dockercfg /root/.dockercfg-deactivated
docker run --rm registry.local/quux/bar | grep -q "Hello, world" || die "Expected 'Hello, world'"

cd /source
./rescoyl-checks docker-all
