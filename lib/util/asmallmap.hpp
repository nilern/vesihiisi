#pragma once

#include <optional>

#include "avec.hpp"

template<typename K, typename V> requires
    (std::is_trivially_destructible<K>{}()) && (std::is_trivially_copyable<K>{}())
            && std::equality_comparable<K>
    && (std::is_trivially_destructible<V>{}()) && (std::is_trivially_copyable<V>{}())
class ASmallMap {
public:
    struct Entry {
        K key;
        V value;
    };

private:
    AVec<Entry> entries_;

    explicit ASmallMap(AVec<Entry>&& entries) : entries_{std::move(entries)} {}

public:
    ASmallMap(Arena* arena, size_t capacity) : entries_{arena, capacity} {}

    explicit ASmallMap(Arena* arena) : entries_{arena} {}

    ASmallMap clone() const { return ASmallMap{entries_.clone()}; }

    std::optional<V> tryGet(K const& k) const {
        auto const it = std::find_if(entries_.begin(), entries_.end(),
                                     [&](Entry const& entry) { return entry.key == k; });
        return it != entries_.end() ? std::optional{it->value} : std::nullopt;
    }

    void set(K const& k, V const& v) {
        auto const it = std::find_if(entries_.begin(), entries_.end(),
                                     [&](Entry const& entry) { return entry.key == k; });
        if (it != entries_.end()) {
            it->value = v;
        } else {
            entries_.push(Entry{k, v});
        }
    }

    using const_iterator = Entry const*;
    const_iterator begin() const { return entries_.begin(); }
    const_iterator end() const { return entries_.end(); }
};
