#include "object.h"

static uint64_t const payloadWidth = 48;
static uint64_t const payloadMask = ((uint64_t)1 << payloadWidth) - 1; // `payloadWidth` ones

static uint64_t const tagMask = (uint64_t)0x7fff << payloadWidth;

static uint64_t const nonFlonumTag = (uint64_t)0x7ffc << payloadWidth;

// OPTIMIZE: Utilize unused bits
static uint64_t const fixnumTag = nonFlonumTag | ((uint64_t)TYPE_FIXNUM << payloadWidth);
static uint64_t const charTag = nonFlonumTag | ((uint64_t)TYPE_CHAR << payloadWidth);
static uint64_t const boolTag = nonFlonumTag | ((uint64_t)TYPE_BOOL << payloadWidth);
// By using 0b00 for pointers we avoid any conflict with actual NaN:s as we do not want null
// pointers anyway:
static uint64_t const heapedTag = nonFlonumTag | ((uint64_t)0b00 << payloadWidth);

static ORef const Default = {0}; // 0.0

static Bool const True = {boolTag | (uint64_t)true};
static Bool const False = {boolTag | (uint64_t)false};

// Just needs to be distinguishable from both `Header` and flex count fixnum. Using 0.0 has the
// added advantage of requiring no initialization on allocation (as heap is already zeroed):
static ORef const AlignmentHole = {0};

static ORef const Tombstone = {boolTag | (uint64_t)false};

static uint64_t const markBit = (uint64_t)0b01 << payloadWidth;

static const size_t objectMinAlign = alignof(Header);
