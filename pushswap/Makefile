##
## pushswap project
## make
## File description:
## make
##

NAME	=	pushswap_checker

CC		=	ghc -o --make $(NAME)

RM		=	rm -f

SRCS	=	$(wildcard *.hs)

CFLAGS +=	-std=gnu11 -Wall -Wextra

all: $(NAME)

$(NAME):
	 $(CC) $(SRCS)

clean:
	$(RM) $(NAME)
	$(RM) *~
	$(RM) *.o
	$(RM) *.hi

fclean: clean
	 $(RM) $(NAME)

re: fclean all

.PHONY: all clean fclean re