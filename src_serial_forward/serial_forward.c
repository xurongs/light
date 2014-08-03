#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/sysinfo.h>
#include <fcntl.h>
#include <termios.h>
#include <errno.h>
#include <string.h>

#define ARRAY_SIZE(array) (sizeof(array) / sizeof(array[0]))

typedef struct
{
	int from;
	int to;
} FORWARD;

typedef struct
{
	int cnt;
	FORWARD *items;
} FORWARD_GROUP;

void forward_once(int from, int to)
{
	char buff[32];
	int bytes = read(from, buff, sizeof(buff));
	if (-1 != bytes)
	{
		if (bytes > 0)
		{
			write(to, buff, bytes);
			fsync(to);
			// fprintf(stderr, "%d -> %d : %d bytes\n", from, to, bytes);
		}
		else
		{
			fprintf(stderr, "Close serial port.\n");
			exit(-1);
		}
	}
	else
	{
		fprintf(stderr, "read from %d failed.\n", from);
	}
}

static int max(int a, int b)
{
	return a >= b ? a : b;
}

int forward_group_max_fd(FORWARD_GROUP *group)
{
	int max_fd = 0;
	int i;
	for (i = 0; i < group->cnt; i++)
	{
		max_fd = max(max_fd, group->items[i].from);
	}
	return max_fd;
}

fd_set *forward_group_fd_set(FORWARD_GROUP *group, fd_set *fds)
{
	int i;
	FD_ZERO(fds);		
	for (i = 0; i < group->cnt; i++)
	{
		FD_SET(group->items[i].from, fds);
	}
	return fds;
}

void forward_group_once(FORWARD_GROUP *group, fd_set *read)
{
	int i;
	for (i = 0; i < group->cnt; i++)
	{
		FORWARD *fwd = &group->items[i];
		if (FD_ISSET(fwd->from, read))
		{
			forward_once(fwd->from, fwd->to);
		}
	}	
}

void forward_group(FORWARD_GROUP *group)
{
	while (1)
	{
		fd_set fs_read;

		if (select(
				forward_group_max_fd(group) + 1,
				forward_group_fd_set(group, &fs_read), /* read */
				NULL,                            /* write */
				NULL,                            /* error */
				NULL)                            /* time out */
			> 0)
		{
			forward_group_once(group, &fs_read);
		}
		else
		{
			fprintf(stderr, "select error!\n");
		}
	}
}


void set_serial(int fd)
{
	struct termios opt;
	
	tcgetattr(fd, &opt);
	
	cfsetispeed(&opt, B9600);
	cfsetospeed(&opt, B9600);
	
	opt.c_cflag &= ~PARENB;
	opt.c_iflag &= ~INPCK; 

	/* 1 stop bit */
	opt.c_cflag &= ~CSTOPB;

	/* 8 bit*/
	opt.c_cflag &= ~CSIZE; 
	opt.c_cflag |= CS8;

	opt.c_cflag |= (CLOCAL | CREAD);

	opt.c_lflag &= ~(ICANON | ECHO | ECHOE | ISIG);

	opt.c_oflag &= ~OPOST;
	opt.c_oflag &= ~(ONLCR | OCRNL);

	opt.c_iflag &= ~(ICRNL | INLCR);
	opt.c_iflag &= ~(IXON | IXOFF | IXANY);

	opt.c_cc[VTIME] = 10;
	opt.c_cc[VMIN] = 9;

	tcflush(fd, TCIFLUSH);
	tcsetattr(fd, TCSANOW, &opt);
	
}

void forward_serial(const char *dev)
{
	int uart = open(dev, O_RDWR | O_NOCTTY | O_NDELAY);
	if (-1 != uart)
	{
		set_serial(uart);
		
		FORWARD forward[] = {
			{uart, STDOUT_FILENO},
			{STDIN_FILENO, uart}
			};
		FORWARD_GROUP group = {.cnt = ARRAY_SIZE(forward), .items = forward};
		forward_group(&group);
		
		close(uart);
	}
	else
	{
		fprintf(stderr, "open uart failed(%d).\n", errno);
	}
}

int main(int argc, const char *argv[])
{
	const char *uart_dev = "/dev/ttySAC2";
	if (argc >= 2)
	{
		uart_dev = argv[1];
	}
	fprintf(stderr, "Start serial(%s) forward...\n", uart_dev);
	forward_serial(uart_dev);		
	return -1;
}
