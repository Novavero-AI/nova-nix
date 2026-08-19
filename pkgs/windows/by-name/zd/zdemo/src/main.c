#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <zlib.h>

int main(void)
{
    const char *msg =
        "nova-nix linked zlib from source and round-tripped this string.";
    uLong srcLen = (uLong)strlen(msg) + 1;
    uLong compLen = compressBound(srcLen);
    Bytef *comp = malloc(compLen);
    Bytef *back = malloc(srcLen);
    uLong backLen = srcLen;

    if (comp == NULL || back == NULL) {
        fprintf(stderr, "out of memory\n");
        return 1;
    }
    if (compress(comp, &compLen, (const Bytef *)msg, srcLen) != Z_OK) {
        fprintf(stderr, "compress failed\n");
        return 1;
    }
    if (uncompress(back, &backLen, comp, compLen) != Z_OK) {
        fprintf(stderr, "uncompress failed\n");
        return 1;
    }

    printf("linked zlib version: %s\n", zlibVersion());
    printf("original:   %lu bytes\n", (unsigned long)srcLen);
    printf("compressed: %lu bytes\n", (unsigned long)compLen);
    printf("round-trip: %s\n", strcmp(msg, (const char *)back) == 0 ? "OK" : "MISMATCH");
    printf("recovered:  %s\n", (const char *)back);

    free(comp);
    free(back);
    return 0;
}
