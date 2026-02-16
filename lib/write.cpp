#include "write.hpp"

#include "../deps/utf8proc/utf8proc.h"

namespace {

void write(RT const* state, FILE* dest, ORef v) {
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
        if (isa<String>(*state, v)) {
            HRef<String> const s = HRef<String>::fromUnchecked(v);
            
            Str const str = s->str();
            fprintf(dest, "\"%.*s\"", (int)str.len, str.data);
        } else if (isa<Symbol>(*state, v)) {
            HRef<Symbol> const s = HRef<Symbol>::fromUnchecked(v);

            ORef const anyName = s->name;
            if (isHeaped(anyName)) {
                Str const nameStr = HRef<String>::fromUnchecked(anyName)->str();
                fprintf(dest, "%.*s", (int)nameStr.len, nameStr.data);
            } else {
                fprintf(dest, "G__%lu", static_cast<uint64_t>(s->hash.val()));
            }
        } else if (isa<Pair>(*state, v)) {
            auto pair = HRef<Pair>::fromUnchecked(v);
            
            fputc('(', dest);
            write(state, dest, pair->car().get());
            
            for (ORef tail = pair->cdr().get(); true; tail = pair->cdr().get()) {
                if (isa<Pair>(*state, tail)) {
                    pair = HRef<Pair>::fromUnchecked(tail);
                    fputc(' ', dest);
                    write(state, dest, pair->car().get());
                } else if (isEmptyList(state, tail)) {
                    break;
                } else {
                    printf(" . ");
                    write(state, dest, tail);
                    break;
                }
            }
            
            fputc(')', dest);
        } else if (isEmptyList(state, v)) {
            fprintf(dest, "()");
        } else if (isa(state, state->types.array, v)) {
            ORefSpan const vs = HRef<Array>::fromUnchecked(v)->flexItems();

            fprintf(dest, "#<array");

            size_t const count = vs.size();
            for (size_t i = 0; i < count; ++i) {
                fputc(' ', dest);
                write(state, dest, vs[i]);
            }

            putc('>', dest);
        } else if (isa(state, state->types.arrayMut, v)) {
            ORefSpan const vs = HRef<ArrayMut>::fromUnchecked(v)->flexItems();

            fprintf(dest, "#<array!");

            size_t const count = vs.size();
            for (size_t i = 0; i < count; ++i) {
                fputc(' ', dest);
                write(state, dest, vs[i]);
            }

            putc('>', dest);
        } else if (isa(state, state->types.method, v)) {
            auto const method = HRef<Method>::fromUnchecked(v);

            fprintf(dest, "#<method");
            ORef const maybeName = method->maybeName;
            if (isHeaped(maybeName)) {
                putc(' ', dest);
                write(state, dest, maybeName);
            }
            putc('>', dest);
        } else if (isa<Closure>(*state, v)) {
            auto const closure = HRef<Closure>::fromUnchecked(v);

            fprintf(dest, "#<fn");

            // TODO: DRY with #<method ... directly above:
            if (isa<Method>(*state, closure->method)) {
                auto const method = HRef<Method>::fromUnchecked(closure->method);
                ORef const maybeName = method->maybeName;
                if (isHeaped(maybeName)) {
                    putc(' ', dest);
                    write(state, dest, maybeName);
                }
            }

            putc('>', dest);
        } else if (isa<Multimethod>(*state, v)) {
            auto const multimethod = HRef<Multimethod>::fromUnchecked(v);

            fputs("#<multimethod", dest);

            if (isHeaped(multimethod->maybeName)) {
                putc(' ', dest);
                write(state, dest, multimethod->maybeName);
            }

            putc('>', dest);
        } else if (isa<Type>(*state, v)) {
            auto const type = HRef<Type>::fromUnchecked(v);

            fprintf(dest, "#<type ");
            write(state, dest, type->name);
            putc('>', dest);
        } else if (isa<Pointer>(*state, v)) {
            auto const ptr = HRef<Pointer>::fromUnchecked(v);

            fprintf(dest, "#<pointer %p>", ptr->val);
        } else if (isa(state, state->types.fatalError, v)) {
            auto const err = HRef<FatalError>::fromUnchecked(v);

            fputs("#<fatal-error ", dest);

            write(state, dest, err->name);

            ORefSpan const irritants = err->irritants();
            size_t const count = irritants.size();
            for (size_t i = 0; i < count; ++i) {
                putc(' ', dest);
                write(state, dest, irritants[i]);
            }

            putc('>', dest);
        } else if (isa(state, state->types.unboundError, v)) {
            auto const err = HRef<UnboundError>::fromUnchecked(v);

            fputs("#<unbound-error ", dest);
            write(state, dest, err->name);
            putc('>', dest);
        } else if (isa<TypeError>(*state, v)) {
            auto const err = HRef<TypeError>::fromUnchecked(v);

            fputs("#<type-error ", dest);
            write(state, dest, err->type);
            putc(' ', dest);
            write(state, dest, err->val);
            putc('>', dest);
        } else if (isa(state, state->types.arityError, v)) {
            auto const err = HRef<ArityError>::fromUnchecked(v);

            fputs("#<arity-error ", dest);
            write(state, dest, err->callee);
            putc(' ', dest);
            write(state, dest, err->callArgc);
            putc('>', dest);
        } else if (isa(state, state->types.inapplicableError, v)) {
            auto const err = HRef<InapplicableError>::fromUnchecked(v);

            fputs("#<inapplicable-error ", dest);
            write(state, dest, err->callee);
            putc('>', dest);
        } else {
            auto const type = typeOf(state, v);

            fputs("#<", dest);
            write(state, dest, type->name);
            putc('>', dest);
        }
        break;
    }
}

} // namespace
