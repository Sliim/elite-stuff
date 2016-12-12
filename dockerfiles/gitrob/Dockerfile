FROM ruby:2.3-slim

RUN apt-get update && \
	apt-get -y install libpq-dev build-essential

RUN gem install bundler
RUN gem install github_api -v 0.14.0
RUN gem install gitrob -v 1.1.0
RUN touch /usr/local/bundle/gems/gitrob-1.1.0/agreement.txt

ENTRYPOINT /usr/local/bundle/bin/gitrob
CMD server --bind-address 0.0.0.0
