# Usage:
#  - docker build . -t teamserver_custom
#  - docker run -p 6661:6661 -v $(pwd)/armitage:/root/.armitage teamserver_custom 10.13.37.10 5555 msf pentestlab-test 6661 10.13.37.10

FROM openjdk:8
RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y git-core ant zip
RUN apt-get clean; apt-get autoclean; apt-get autoremove -y
RUN rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN git clone https://github.com/Sliim/armitage /armitage
WORKDIR /armitage
RUN sh /armitage/package.sh

COPY teamserver /armitage/teamserver

ENTRYPOINT ["/armitage/teamserver"]
CMD ["0.0.0.0", "55551", "msf", "teamserver-password", "55553"]
