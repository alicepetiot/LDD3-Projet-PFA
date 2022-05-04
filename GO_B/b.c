#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#define TAILLE 100

char rand_char() {
    return 'A' + rand() % 26;
}

void init_tab(char* tab) {
    for (int i = 0; i < TAILLE; i++) {
        tab[i] = (rand() % 40 == 0) ? rand_char() : ' ';
    }
}

void maj_tab(char* tab) {
    for (int i = 0; i < TAILLE; i++) {
        if (tab[i] == ' ' && rand() % 50 == 0) {
            tab[i] = rand_char();
        } else if (tab[i] != ' ') {
            if (rand() % 10 == 0) {
                tab[i] = ' ';
            } else {
                tab[i] = rand_char();
            }
        }
    }
}

void affiche_tab(char* tab) {
    for (int i = 0; i < TAILLE; i++) {
        putchar(tab[i]);
    }
    putchar('\n');
}

int main() {

    srand(time(NULL));

    char tab[TAILLE];

    init_tab(tab);

    printf("\x1b[92m");

    while (1) {
        affiche_tab(tab);
        maj_tab(tab);
        usleep(10000);
    }

    return 0;
}
