#include "object.hpp"

#include "state.hpp"

namespace {

bool InputFile::open(State* state, HRef<InputFile>& res, HRef<String> filename) {
    UTF8InputFile file;
    if (!UTF8InputFile::open(file, filename->str())) { return false; }

    res = createInputFile(state, std::move(file));
    return true;
}

} // namespace
