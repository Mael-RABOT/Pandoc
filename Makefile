##
## EPITECH PROJECT, 2023
## B-FUN-400-LYN-4-1-mypandoc-mael.rabot
## File description:
## Makefile
##

NAME		=	mypandoc

BINARY_PATH	:=	$(shell stack path --local-install-root)

all:
	stack build
	cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)

clean:
	stack clean

fclean: clean
	rm -f $(NAME)

re: fclean all
