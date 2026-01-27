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
            
            Str const str = s.ptr()->str();
            fprintf(dest, "\"%.*s\"", (int)str.len, str.data);
        } else if (isSymbol(state, v)) {
            HRef<Symbol> const s = HRef<Symbol>::fromUnchecked(v);

            Str const name = s.ptr()->name();
            fprintf(dest, "%.*s", (int)name.len, name.data);
        } else if (isPair(state, v)) {
            Pair const* pair = HRef<Pair>::fromUnchecked(v).ptr();
            
            fputc('(', dest);
            print(state, dest, pair->car);
            
            for (ORef tail = pair->cdr; true; tail = pair->cdr) {
                if (isPair(state, tail)) {
                    pair = HRef<Pair>::fromUnchecked(tail).ptr();
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
            Slice<ORef const> const vs = HRef<Array>::fromUnchecked(v).ptr()->flexItems();

            fprintf(dest, "#<array");

            size_t const count = vs.count;
            for (size_t i = 0; i < count; ++i) {
                fputc(' ', dest);
                print(state, dest, vs[i]);
            }

            putc('>', dest);
        } else if (isa(state, state->types.arrayMut, v)) {
            Slice<ORef const> const vs = HRef<ArrayMut>::fromUnchecked(v).ptr()->flexItems();

            fprintf(dest, "#<array!");

            size_t const count = vs.count;
            for (size_t i = 0; i < count; ++i) {
                fputc(' ', dest);
                print(state, dest, vs[i]);
            }

            putc('>', dest);
        } else if (isa(state, state->types.method, v)) {
            Method const* const method = HRef<Method>::fromUnchecked(v).ptr();

            fprintf(dest, "#<method");
            ORef const maybeName = method->maybeName;
            if (isHeaped(maybeName)) {
                putc(' ', dest);
                print(state, dest, maybeName);
            }
            putc('>', dest);
        } else if (isClosure(state, v)) {
            Closure const* const closure = HRef<Closure>::fromUnchecked(v).ptr();

            fprintf(dest, "#<fn");

            // TODO: DRY with #<method ... directly above:
            if (isMethod(state, closure->method)) {
                Method const* const method = HRef<Method>::fromUnchecked(closure->method).ptr();
                ORef const maybeName = method->maybeName;
                if (isHeaped(maybeName)) {
                    putc(' ', dest);
                    print(state, dest, maybeName);
                }
            }

            putc('>', dest);
        } else if (isMultimethod(state, v)) {
            Multimethod const* const multimethod = HRef<Multimethod>::fromUnchecked(v).ptr();

            fputs("#<multimethod", dest);

            if (isHeaped(multimethod->maybeName)) {
                putc(' ', dest);
                print(state, dest, multimethod->maybeName);
            }

            putc('>', dest);
        } else if (isType(state, v)) {
            Type const* const type = HRef<Type>::fromUnchecked(v).ptr();

            fprintf(dest, "#<type ");
            print(state, dest, type->name.oref());
            putc('>', dest);
        } else if (isa(state, state->types.fatalError, v)) {
            FatalError const* const err = HRef<FatalError>::fromUnchecked(v).ptr();

            fputs("#<fatal-error ", dest);

            print(state, dest, err->name.oref());

            Slice<ORef const> const irritants = err->irritants();
            size_t const count = irritants.count;
            for (size_t i = 0; i < count; ++i) {
                putc(' ', dest);
                print(state, dest, irritants[i]);
            }

            putc('>', dest);
        } else if (isa(state, state->types.unboundError, v)) {
            UnboundError const* const err = HRef<UnboundError>::fromUnchecked(v).ptr();

            fputs("#<unbound-error ", dest);
            print(state, dest, err->name.oref());
            putc('>', dest);
        } else if (isTypeError(state, v)) {
            TypeError const* const err = HRef<TypeError>::fromUnchecked(v).ptr();

            fputs("#<type-error ", dest);
            print(state, dest, err->type.oref());
            putc(' ', dest);
            print(state, dest, err->val);
            putc('>', dest);
        } else if (isa(state, state->types.arityError, v)) {
            ArityError const* const err = HRef<ArityError>::fromUnchecked(v).ptr();

            fputs("#<arity-error ", dest);
            print(state, dest, err->callee.oref());
            putc(' ', dest);
            print(state, dest, err->callArgc.oref());
            putc('>', dest);
        } else if (isa(state, state->types.inapplicableError, v)) {
            InapplicableError const* const err = HRef<InapplicableError>::fromUnchecked(v).ptr();

            fputs("#<inapplicable-error ", dest);
            print(state, dest, err->callee.oref());
            putc('>', dest);
        } else {
            Type const* const type = typeOf(state, v).ptr();

            fputs("#<", dest);
            print(state, dest, type->name);
            putc('>', dest);
        }
        break;
    }
}

} // namespace
