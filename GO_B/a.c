#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {

    while (1) {

        for (int i = 0; i < 50; i++) {
            printf("\x1b[38;2;%d;%d;%dma\x1b[0m", rand() % 256, rand() % 256, rand() % 256);
        }

        putchar('\n');
    }

    return 0;
}