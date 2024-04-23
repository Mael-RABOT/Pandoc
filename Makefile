##
## EPITECH PROJECT, 2023
## B-FUN-400-LYN-4-1-mypandoc-mael.rabot
## File description:
## Makefile
##

NAME		=	mypandoc

BINARY_PATH	=	`stack path --local-install-root`/bin

all:
	stack build --allow-different-user
	cp $(BINARY_PATH)/$(NAME)-exe ./$(NAME)

clean:
	stack clean

fclean: clean
	rm -f $(NAME)

re: fclean all
