#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>

#include <err.h>
#include <pthread.h>
#include <unistd.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

pthread_mutex_t counter_mutex = PTHREAD_MUTEX_INITIALIZER;
int counter_value = 0;
int nthreads = 1000;
int opt_a = 0;
int opt_m = 0;


#ifndef __STDC_NO_ATOMICS__
#include <stdatomic.h>

atomic_int atomic_counter_value = ATOMIC_VAR_INIT(0);

int
atomic_count(void)
{
	return atomic_fetch_add(&atomic_counter_value, 1);
}
#else
int
atomic_count(void)
{
	return 0;
}
#endif

int
mutex_count(void)
{
	int ret;

	pthread_mutex_lock(&counter_mutex);
	ret = counter_value++;
	pthread_mutex_unlock(&counter_mutex);
	return ret;
}

int
unsafe_count(void)
{
	return counter_value++;
}

void *
serve(void *lsockp)
{
	struct sockaddr_storage ss;
	socklen_t sslen;
	ssize_t nr;
	int k, lsock, sock, wlen;
	char readbuf[1024], writebuf[1024];

	lsock = *(int *)lsockp;
	while (1) {
		sslen = sizeof(ss);
		sock = accept(lsock, (void *)&ss, &sslen);
		nr = read(sock, readbuf, sizeof(readbuf));

		if (opt_a)
			k = atomic_count();
		else if (opt_m)
			k = mutex_count();
		else
			k = unsafe_count();

		wlen = snprintf(writebuf, sizeof(writebuf), "%d\n", k);

		write(sock, writebuf, wlen);
		close(sock);
	}
	return NULL;
}

int
main(int argc, char **argv)
{
	struct addrinfo hints, *res;
	pthread_t *threads;
	int ch, lsock, on;

	while ((ch = getopt(argc, argv, "amt:u")) != -1)
		switch (ch) {
		case 'a':
#ifdef __STDC_NO_ATOMICS__
			errx(1, "your compiler does not support <stdatomic.h>");
#endif
			opt_a = 1;
			break;
		case 'm':
			opt_m = 1;
			break;
		case 't':
			nthreads = atoi(optarg);
			break;
		case 'u':
			/* unsafe operation is default */
			break;
		}
	argc -= optind;
	argv += optind;

	memset(&hints, 0, sizeof(hints));
	hints.ai_family = AF_INET;
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_flags = AI_NUMERICHOST | AI_PASSIVE;
	getaddrinfo("0.0.0.0", "8888", &hints, &res);
	lsock = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
	if (lsock == -1)
		err(1, "socket");
	on = 1;
	if (setsockopt(lsock, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on)) == -1)
		err(1, "setsockopt");
	if (bind(lsock, res->ai_addr, res->ai_addrlen) == -1)
		err(1, "bind");
	if (listen(lsock, 100) == -1)
		err(1, "listen");

	threads = calloc(nthreads, sizeof(pthread_t));
	for (int i = 0; i < nthreads; i++) {
		if (pthread_create(&threads[i], NULL, serve, &lsock) != 0)
			err(1, "pthread_create");
	}
	for (int i = 0; i < nthreads; i++) {
		void *nullp;
		pthread_join(threads[i], &nullp);
	}

	return 0;
}
