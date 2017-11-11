FROM fusionapp/debian-stack-run
ADD ./ /
WORKDIR "/srv/catcher-in-the-rye"
CMD ["/usr/local/bin/catcher-in-the-rye"]
