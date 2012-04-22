/*
 * Copyright (c) 2012 Tomoyuki Sahara <sahara@surt.net>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/stat.h>
#include <net/bpf.h>
#include <net/if.h>

#include <err.h>
#include <fcntl.h>
#include <unistd.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int bpf;
char *ifname;

#define PKTBUFSIZE	2048
#define QMAX		16

struct pktqueue {
	struct pktbuf *first;
	struct pktbuf *last;
	unsigned int len;
};

struct pktbuf {
	struct pktbuf *next;
	unsigned char buf[PKTBUFSIZE];
	unsigned int size;
};

struct pktqueue inq = { NULL, NULL };
struct pktqueue outq = { NULL, NULL };


static int pktqueue_full(struct pktqueue *);

struct pktbuf *
pktbuf_new(void)
{
	struct pktbuf *p;

	p = malloc(sizeof(struct pktbuf));
	p->next = NULL;
	p->size = 0;
	return p;
}

void
pktbuf_free(struct pktbuf *p)
{
	if (p->next != NULL)
		warnx("pktbuf->next is not free");
	free(p);
}

void
pktqueue_init(struct pktqueue *q)
{
	q->first = q->last = NULL;
	q->len = 0;
}

int
pktqueue_empty(struct pktqueue *q)
{
	return q->len == 0;
}

struct pktbuf *
pktqueue_dequeue(struct pktqueue *q)
{
	struct pktbuf *pkt;

	if (q->len == 0)
		return NULL;
	pkt = q->first;
	if (pkt->next != NULL)
		q->first = pkt->next;
	else
		q->first = q->last = NULL;
	q->len--;
	pkt->next = NULL;
	return pkt;
}

void
pktqueue_enqueue(struct pktqueue *q, struct pktbuf *pkt)
{
	if (pktqueue_full(q)) {
		warnx("queue full");
		return;
	}
	pkt->next = NULL;
	if (q->first == NULL)
		q->first = q->last = pkt;
	else {
		q->last->next = pkt;
		q->last = pkt;
	}
	q->len++;
}

static int
pktqueue_full(struct pktqueue *q)
{
	return q->len >= QMAX;
}



int
main(int argc, char **argv)
{
	struct ifreq ifr;
	struct pktbuf *pkt;
	fd_set rfds, wfds;
	struct sockaddr_un sun, sun2;
	socklen_t sunlen;
	unsigned int k;
	int ch, i, n, isock;
	char filename[10];

	while ((ch = getopt(argc, argv, "")) != -1) {
		switch (ch) {
		case '?':
		default:
			break;
		}
	}
	argc -= optind;
	argv += optind;

	if (argc < 1)
		errx(1, "interfame name required");

	ifname = argv[0];
	warnx("tapping %s", ifname);

	for (bpf = -1, i = 0; i < 4; i++) {
		snprintf(filename, sizeof(filename), "/dev/bpf%u", i);
		bpf = open(filename, O_RDWR);
		if (bpf >= 0) 
			break;
		if (errno != EBUSY)
			err(1, "open bpf");
	}
	if (bpf == -1)
		errx(1, "no bpf device available");

	k = PKTBUFSIZE;
	if (ioctl(bpf, BIOCSBLEN, &k) == -1)
		err(1, "ioctl(BIOCSBLEN)");

	memset(&ifr, 0, sizeof(ifr));
	strncpy(ifr.ifr_name, ifname, sizeof(ifr.ifr_name));
	if (ioctl(bpf, BIOCSETIF, &ifr) == -1)
		err(1, "ioctl(BIOCSETIF)");

	k = 1;
	if (ioctl(bpf, BIOCIMMEDIATE, &k) == -1)
		err(1, "ioctl(BIOCIMMEDIATE)");

	k = 1;
	if (ioctl(bpf, BIOCSHDRCMPLT, &k) == -1)
		err(1, "ioctl(BIOCSHDRCMPLT)");

	/* don't want loopback */
	k = 0;
	if (ioctl(bpf, BIOCGSEESENT, &k) == -1)
		err(1, "ioctl(BIOCGSEESENT)");

	if (ioctl(bpf, BIOCPROMISC) == -1)
		err(1, "ioctl(BIOCPROMISC)");


	isock = socket(AF_UNIX, SOCK_STREAM, 0);
	if (isock == -1)
		err(1, "socket isock");

if (0) {
	k = 1;
	if (setsockopt(isock, SOL_SOCKET, SO_REUSEADDR, &k, sizeof(k)) == -1)
		err(1, "setsockopt SO_REUSEADDR");
}

	memset(&sun, 0, sizeof(sun));
	sun.sun_family = AF_UNIX;
	snprintf(sun.sun_path, sizeof(sun.sun_path), "/tmp/ifcat-%s", ifname);
unlink(sun.sun_path);
	if (bind(isock, (struct sockaddr *)&sun, sizeof(sun)) == -1)
		err(1, "bind isock");

	if (chmod(sun.sun_path, S_IRWXU|S_IRWXG|S_IRWXO) == -1)
		err(1, "chmod isock");

	if (listen(isock, 1) == -1)
		err(1, "listen isock");

	sunlen = sizeof(sun2);
	isock = accept(isock, (void *)&sun2, &sunlen);
	if (isock == -1)
		err(1, "accept isock");

	pktqueue_init(&inq);
	pktqueue_init(&outq);

	while (1) {
		FD_ZERO(&rfds);
		FD_ZERO(&wfds);

		if (!pktqueue_empty(&inq))
			FD_SET(isock, &wfds);
		if (!pktqueue_full(&inq))
			FD_SET(bpf, &rfds);
		if (!pktqueue_empty(&outq))
			FD_SET(bpf, &wfds);
		if (!pktqueue_full(&outq))
			FD_SET(isock, &rfds);

		n = select(isock + 1, &rfds, &wfds, NULL, NULL);
		if (n == -1) {
			if (errno == EINTR)
				continue;
			err(1, "poll");
		}

		if (FD_ISSET(isock, &wfds)) {
			pkt = pktqueue_dequeue(&inq);
			i = write(isock, &pkt->size, sizeof(pkt->size));
			if (i == -1)
				err(1, "write to stdout");
			i = write(isock, pkt->buf, pkt->size);
			if (i == -1)
				err(1, "write to stdout");
			pktbuf_free(pkt);
		}

		if (FD_ISSET(bpf, &wfds)) {
			pkt = pktqueue_dequeue(&outq);
			i = write(bpf, pkt->buf, pkt->size);
			if (i == -1)
				err(1, "send");
			pktbuf_free(pkt);
		}

		if (FD_ISSET(bpf, &rfds)) {
			pkt = pktbuf_new();
			i = read(bpf, pkt->buf, sizeof(pkt->buf));
			if (i == -1)
				err(1, "read");
			pkt->size = i;
			pktqueue_enqueue(&inq, pkt);
		}

		if (FD_ISSET(isock, &rfds)) {
			pkt = pktbuf_new();
			i = read(isock, &pkt->size, sizeof(pkt->size));
			if (i == -1)
				err(1, "read from stdin");
			i = read(isock, pkt->buf, pkt->size);
			if (i == -1)
				err(1, "read from stdin");
			pktqueue_enqueue(&inq, pkt);
		}
	}

	exit(0);
}
