#pragma once

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

    V const* tryGet(K const& k) const {
        auto const it = std::find_if(entries_.begin(), entries_.end(),
                                     [&](Entry const& entry) { return entry.key == k; });
        return it != entries_.end() ? &it->value : nullptr;
    }

    V* tryGet(K const& k) {
        auto const it = std::find_if(entries_.begin(), entries_.end(),
                                     [&](Entry const& entry) { return entry.key == k; });
        return it != entries_.end() ? &it->value : nullptr;
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

    using const_iterator = Entry const*;
    const_iterator begin() const { return entries_.data(); }
    const_iterator end() const { return begin() + entries_.size(); }
};
