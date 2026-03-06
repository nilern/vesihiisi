#pragma once

#include <optional>
#include <vector>
#include <algorithm>

template<typename K, typename V> requires std::equality_comparable<K>
class SmallMap {
public:
    struct Entry {
        K key;
        V value;
    };

private:
    std::vector<Entry> entries_;

public:
    SmallMap() : entries_{} {}

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
            entries_.emplace_back(k, v);
        }
    }
};
