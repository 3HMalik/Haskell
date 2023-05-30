##
## Project
## Makefile
## File description:
## Makefile
##

SRC =	DoOp.hs \

OBJ = $(SRC:.hs=.o)

NAME = doop

CFLAGS = -std=gnu11 -W -Wall -Wextra

all: $(NAME)

$(NAME): $(OBJ)
	ghc -o $(NAME) $(OBJ) $(CFLAGS)

clean:
	rm -f $(OBJ)
	rm -f *~

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: clean fclean all re
