# Usage:
#   - docker build . -t ctfpad
#   - docker run -p bindip:1234:1234 -p bindip:1235:1235 ctfpad
#
# TODO: Volume for ctfpad.sqlite file
# TODO: Review Etherpad DB for persistence!

FROM node:6-alpine
RUN apk add --update openssl git sqlite curl

# Clone repos
RUN git clone https://github.com/StratumAuhuur/CTFPad /CTFPad
RUN git clone https://github.com/ether/etherpad-lite /CTFPad/etherpad-lite

# Etherpad setup
WORKDIR /CTFPad/etherpad-lite
RUN bin/installDeps.sh

# CTFPad setup
WORKDIR /CTFPad
RUN mkdir uploads
RUN npm install
RUN openssl req -new -newkey rsa:4096 -days 365 -nodes -x509 -subj "/C=FR/ST=/L=/O=Lab/CN=pentest.lab" -keyout key.pem -out cert.pem

# Volumes
VOLUME ctfpad.sqlite:/CTFPad/ctfpad.sqlite
VOLUME config.json:/CTFPad/config.json
VOLUME settings.json:/CTFPad/etherpad-lite/settings.json

# Final setup
RUN adduser -D -h /CTFPad -H -g CTFPad ctfpad
RUN chown -R ctfpad /CTFPad
USER ctfpad
EXPOSE 1234 1235
ENTRYPOINT ["node", "main.js"]
