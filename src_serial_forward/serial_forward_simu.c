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

struct RCV
{
	char buf[32];
	int cnt;
};

struct STATE
{
	int light;
	int key;
};

struct RCV rcv = {.cnt = 0};
struct STATE state = {.light = 0, .key = 0};

void fill_rcv(char c)
{
	if (rcv.cnt < sizeof(rcv.buf))
	{
		rcv.buf[rcv.cnt++] = c;
	}
	else
	{
		exit(255);
	}
}

void empty_rcv()
{
	rcv.cnt = 0;
}

static void fill_status(char *buf, int value)
{
	int i;
	for (i = 0; i < 3; i++)
	{
		buf[i] = (value & 0x7f);
		value >>= 7;
	}
}

static void send_status(int light, int key)
{
	
	char status[] = {1, 0, 0, 0, 0, 0, 0, 0xff};
	
	fill_status(&status[1], light);
	fill_status(&status[4], key);
	
	write(STDOUT_FILENO, status, sizeof(status));
	fsync(STDOUT_FILENO);
	
}

void process(char c)
{
	if ((c + 1) == 0)
	{
		switch (rcv.buf[0])
		{
		case 1:
			send_status(state.light, state.key);
			break;
		case 2:
			state.light |= (1 << rcv.buf[1]);
			send_status(state.light, state.key);
			break;
		case 3:
			state.light &= ~(1 << rcv.buf[1]);
			send_status(state.light, state.key);
			break;
		default:
			break;
		}
		empty_rcv();
	}
	else
	{
		rcv.buf[rcv.cnt++] = c;
	}
}

static void forward_serial()
{
	char buff[32];
	int bytes;

	while ((bytes = read(STDIN_FILENO, buff, sizeof(buff))) > 0)
	{
		int i;
		for (i = 0; i < bytes; i++)
		{
			process(buff[i]);
		}
	}
}

int main()
{
	forward_serial();
	return 0;
}
