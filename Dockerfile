FROM pascal:1
#FROM 0c3f87a3f519

ENV APP_HOME=/app
WORKDIR $APP_HOME

RUN apt install -y nano

COPY . ./

RUN mkdir /app/src/public_qnotes
RUN mkdir /app/public_qnotes
RUN mkdir /app/src/public
RUN mkdir /app/public

COPY data.json /app/src/public/
COPY data.json /app/src/public_qnotes/
COPY data.json /app/public/
COPY data.json /app/public_qnotes/

ENTRYPOINT bash
#ENTRYPOINT ["/app/src/pascalapp", ">", "/app/log.txt", "2>&1"]
# /app/src/pascalapp > /app/log.txt 2>&1
