#pragma once

typedef struct Cookie {
    int calories;
    char* flavor;
} Cookie;

typedef enum ScratchStatus {
    SCRATCH_OK,
    SCRATCH_NOTOK
} ScratchStatus;

ScratchStatus createCookie(int calories, char* flavor, Cookie** newCookie);
