#include "print.hpp"

#include "../deps/utf8proc/utf8proc.h"

namespace {

void print(State const* state, FILE* dest, ORef v) {
    switch (getTag(v)) {
    case TaggedType::FIXNUM:
        fprintf(dest, "%ld", Fixnum::fromUnchecked(v).val());
        break;
        
    case TaggedType::FLONUM:
        fprintf(dest, "%f", Flonum::fromUnchecked(v).val());
        break;
        
    case TaggedType::CHAR: {
        uint8_t buf[4];
        ssize_t const width = utf8proc_encode_char(Char::fromUnchecked(v).val(), buf);
        // TODO: Avoid POSIX format specifier extension:
        fprintf(dest, "#\"%.*s\"", (int)width, buf);
    }; break;
        
    case TaggedType::BOOL:
        if (Bool::fromUnchecked(v).val()) {
            fprintf(dest, "#t");
        } else {
            fprintf(dest, "#f");
        }
        break;
        
    case TaggedType::HEAPED:
        if (isString(state, v)) {
            HRef<String> const s = HRef<String>::fromUnchecked(v);
            
            Str const str = s->str();
            fprintf(dest, "\"%.*s\"", (int)str.len, str.data);
        } else if (isSymbol(state, v)) {
            HRef<Symbol> const s = HRef<Symbol>::fromUnchecked(v);

            Str const name = s->name();
            fprintf(dest, "%.*s", (int)name.len, name.data);
        } else if (isPair(state, v)) {
            auto pair = HRef<Pair>::fromUnchecked(v);
            
            fputc('(', dest);
            print(state, dest, pair->car);
            
            for (ORef tail = pair->cdr; true; tail = pair->cdr) {
                if (isPair(state, tail)) {
                    pair = HRef<Pair>::fromUnchecked(tail);
                    fputc(' ', dest);
                    print(state, dest, pair->car);
                } else if (isEmptyList(state, tail)) {
                    break;
                } else {
                    printf(" . ");
                    print(state, dest, tail);
                    break;
                }
            }
            
            fputc(')', dest);
        } else if (isEmptyList(state, v)) {
            fprintf(dest, "()");
        } else if (isa(state, state->types.array, v)) {
            Slice<ORef const> const vs = HRef<Array>::fromUnchecked(v)->flexItems();

            fprintf(dest, "#<array");

            size_t const count = vs.count;
            for (size_t i = 0; i < count; ++i) {
                fputc(' ', dest);
                print(state, dest, vs[i]);
            }

            putc('>', dest);
        } else if (isa(state, state->types.arrayMut, v)) {
            Slice<ORef const> const vs = HRef<ArrayMut>::fromUnchecked(v)->flexItems();

            fprintf(dest, "#<array!");

            size_t const count = vs.count;
            for (size_t i = 0; i < count; ++i) {
                fputc(' ', dest);
                print(state, dest, vs[i]);
            }

            putc('>', dest);
        } else if (isa(state, state->types.method, v)) {
            auto const method = HRef<Method>::fromUnchecked(v);

            fprintf(dest, "#<method");
            ORef const maybeName = method->maybeName;
            if (isHeaped(maybeName)) {
                putc(' ', dest);
                print(state, dest, maybeName);
            }
            putc('>', dest);
        } else if (isClosure(state, v)) {
            auto const closure = HRef<Closure>::fromUnchecked(v);

            fprintf(dest, "#<fn");

            // TODO: DRY with #<method ... directly above:
            if (isMethod(state, closure->method)) {
                auto const method = HRef<Method>::fromUnchecked(closure->method);
                ORef const maybeName = method->maybeName;
                if (isHeaped(maybeName)) {
                    putc(' ', dest);
                    print(state, dest, maybeName);
                }
            }

            putc('>', dest);
        } else if (isMultimethod(state, v)) {
            auto const multimethod = HRef<Multimethod>::fromUnchecked(v);

            fputs("#<multimethod", dest);

            if (isHeaped(multimethod->maybeName)) {
                putc(' ', dest);
                print(state, dest, multimethod->maybeName);
            }

            putc('>', dest);
        } else if (isType(state, v)) {
            auto const type = HRef<Type>::fromUnchecked(v);

            fprintf(dest, "#<type ");
            print(state, dest, type->name);
            putc('>', dest);
        } else if (isa(state, state->types.fatalError, v)) {
            auto const err = HRef<FatalError>::fromUnchecked(v);

            fputs("#<fatal-error ", dest);

            print(state, dest, err->name);

            Slice<ORef const> const irritants = err->irritants();
            size_t const count = irritants.count;
            for (size_t i = 0; i < count; ++i) {
                putc(' ', dest);
                print(state, dest, irritants[i]);
            }

            putc('>', dest);
        } else if (isa(state, state->types.unboundError, v)) {
            auto const err = HRef<UnboundError>::fromUnchecked(v);

            fputs("#<unbound-error ", dest);
            print(state, dest, err->name);
            putc('>', dest);
        } else if (isTypeError(state, v)) {
            auto const err = HRef<TypeError>::fromUnchecked(v);

            fputs("#<type-error ", dest);
            print(state, dest, err->type);
            putc(' ', dest);
            print(state, dest, err->val);
            putc('>', dest);
        } else if (isa(state, state->types.arityError, v)) {
            auto const err = HRef<ArityError>::fromUnchecked(v);

            fputs("#<arity-error ", dest);
            print(state, dest, err->callee);
            putc(' ', dest);
            print(state, dest, err->callArgc);
            putc('>', dest);
        } else if (isa(state, state->types.inapplicableError, v)) {
            auto const err = HRef<InapplicableError>::fromUnchecked(v);

            fputs("#<inapplicable-error ", dest);
            print(state, dest, err->callee);
            putc('>', dest);
        } else {
            auto const type = typeOf(state, v);

            fputs("#<", dest);
            print(state, dest, type->name);
            putc('>', dest);
        }
        break;
    }
}

} // namespace
