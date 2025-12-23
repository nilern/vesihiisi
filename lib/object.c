#include "object.h"

static uintptr_t const tag_width = 3;
static uintptr_t const tag_bits = (1 << tag_width) - 1; // `tag_width` ones

static uintptr_t const markBit = 0b10;

static Fixnum const Zero = {0};
static Fixnum const One = {(uintptr_t)1 << tag_width | (uintptr_t)TAG_FIXNUM};

static Bool const True = {((uintptr_t)true << tag_width) | (uintptr_t)TAG_BOOL};
static Bool const False = {((uintptr_t)false << tag_width) | (uintptr_t)TAG_BOOL};

static ORef const AlignmentHole = {(uintptr_t)(void*)nullptr | (uintptr_t)TAG_HEAPED};

static const size_t objectMinAlign = alignof(Header);
