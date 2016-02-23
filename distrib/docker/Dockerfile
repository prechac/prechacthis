FROM debian:jessie


RUN set -x \
    && apt-get update --quiet \
    && apt-get install --yes --no-install-recommends swi-prolog apache2 libapache2-mod-php5 \
    && apt-get clean 

RUN mkdir /var/run/sshd
EXPOSE 80
CMD ["bash", "-c", "rm -f /var/run/apache2/apache2.pid \
    && source /etc/apache2/envvars \
    && exec /usr/sbin/apache2 -DFOREGROUND"]
