FROM nhive/ubuntu-16.04
RUN apt-get update
RUN apt-get install -y tomcat8
COPY Hello.war /var/lib/tomcat8/webapps/
COPY start-tc.sh /tmp/
CMD /tmp/start-tc.sh
EXPOSE 8080