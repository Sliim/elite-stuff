# WIP: It doesn't work, problem with envvars LANG & cie

FROM kalilinux/kali-linux-docker
RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y metasploit-framework tmux locales
RUN apt-get clean; apt-get autoclean; apt-get autoremove -y
RUN rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN locale-gen en_US.UTF-8
RUN dpkg-reconfigure locales

COPY startup.rc /usr/share/metasploit-framework/startup.rc
COPY entrypoint.sh /usr/share/metasploit-framework/entrypoint.sh
# VOLUME database.yml:/usr/share/metasploit-framework/config/database.yml
# VOLUME msf4:/root/.msf4

ENTRYPOINT ["/usr/share/metasploit-framework/entrypoint.sh"]
