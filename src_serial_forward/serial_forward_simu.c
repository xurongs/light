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


void forward_serial()
{
	char buff[32];
	int bytes;

	while ((bytes = read(STDIN_FILENO, buff, sizeof(buff))) > 0)
	{
		const char status[] = {1, 0, 0, 0, 0, 0, 0, 0xff};
		write(STDOUT_FILENO, status, sizeof(status));
		fsync(STDOUT_FILENO);
	}
}

int main()
{
	forward_serial();
	return 0;
}
