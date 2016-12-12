# Usage:
# - docker build . -t msfrpc_git
# - docker run -dit msfrpc_git
# - docker run -dit -v $(pwd)/database.yml:/msf/config/database.yml \
#                   -v $(pwd)/msf4:/root/.msf4 \
#                   -e MSFRPC_PASS=foo msfrpc_git
#
# Attach to msfconsole session:
# - docker exec -it CID script -q -c "tmux a -t 0" /dev/null

FROM ruby:2.3

RUN apt-get update && apt-get -y install \
  build-essential zlib1g zlib1g-dev \
  libxml2 libxml2-dev libxslt-dev locate \
  libreadline6-dev libcurl4-openssl-dev git-core \
  libssl-dev libyaml-dev openssl autoconf libtool \
  ncurses-dev bison curl wget xsel postgresql \
  postgresql-contrib libpq-dev \
  libapr1 libaprutil1 libsvn1 \
  libpcap-dev libsqlite3-dev libgmp3-dev \
  tmux

RUN git clone https://github.com/rapid7/metasploit-framework /msf
WORKDIR /msf
RUN git checkout 4.13.2
RUN bundle install

COPY entrypoint.sh entrypoint.sh
COPY startup.rc startup.rc

VOLUME database.yml:/msf/config/database.yml
VOLUME msf4:/root/.msf4

ENTRYPOINT ["./entrypoint.sh"]
