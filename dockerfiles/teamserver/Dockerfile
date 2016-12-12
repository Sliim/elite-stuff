FROM kalilinux/kali-linux-docker
RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y armitage
RUN apt-get clean; apt-get autoclean; apt-get autoremove -y
RUN rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
ENTRYPOINT ["/usr/bin/teamserver"]
CMD ["0.0.0.0", "teamserver_password"]
