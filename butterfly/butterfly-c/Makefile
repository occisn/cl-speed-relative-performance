CC = gcc
CFLAGS = -Iinclude -Wall -Wextra -g -mavx -mfma -fopenmp -O3
LDFLAGS = -fopenmp
SRC = $(wildcard *.c)
OBJ = $(SRC:.c=.o)
BIN = butterfly-c

$(BIN): $(OBJ)
	$(CC) $(LDFLAGS) $(OBJ) -o $(BIN)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f *.o $(BIN)
