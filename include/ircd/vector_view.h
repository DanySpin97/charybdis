// Matrix Construct
//
// Copyright (C) Matrix Construct Developers, Authors & Contributors
// Copyright (C) 2016-2018 Jason Volk <jason@zemos.net>
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice is present in all copies. The
// full license for this software is available in the LICENSE file.

#pragma once
#define HAVE_IRCD_VECTOR_VIEW_H

namespace ircd
{
	template<class T> struct vector_view;
}

template<class T>
struct ircd::vector_view
{
	using value_type = T;
	using pointer = value_type *;
	using reference = value_type &;
	using difference_type = size_t;
	using iterator = value_type *;
	using const_iterator = const value_type *;

	value_type *_data                            { nullptr                                         };
	value_type *_stop                            { nullptr                                         };

  public:
	value_type *data() const                     { return _data;                                   }
	size_t size() const                          { return std::distance(_data, _stop);             }
	bool empty() const                           { return !size();                                 }

	const_iterator begin() const                 { return data();                                  }
	const_iterator end() const                   { return _stop;                                   }
	const_iterator cbegin()                      { return data();                                  }
	const_iterator cend()                        { return _stop;                                   }
	iterator begin()                             { return data();                                  }
	iterator end()                               { return _stop;                                   }

	value_type &operator[](const size_t &pos) const
	{
		return *(data() + pos);
	}

	value_type &at(const size_t &pos) const
	{
		if(unlikely(pos >= size()))
			throw std::out_of_range("vector_view::range_check");

		return operator[](pos);
	}

	vector_view(value_type *const &start, value_type *const &stop)
	:_data{start}
	,_stop{stop}
	{}

	vector_view(value_type *const &start, const size_t &size)
	:vector_view(start, start + size)
	{}

	vector_view(const std::initializer_list<value_type> &list)
	:vector_view(std::begin(list), std::end(list))
	{}

	template<class U,
	         class A>
	vector_view(const std::vector<U, A> &v)
	:vector_view(v.data(), v.size())
	{}

	template<class U,
	         class A>
	vector_view(std::vector<U, A> &v)
	:vector_view(v.data(), v.size())
	{}

	template<size_t SIZE>
	vector_view(value_type (&buffer)[SIZE])
	:vector_view(buffer, SIZE)
	{}

	template<class U,
	         size_t SIZE>
	vector_view(const std::array<U, SIZE> &array)
	:vector_view(array.data(), array.size())
	{}

	template<size_t SIZE>
	vector_view(std::array<value_type, SIZE> &array)
	:vector_view(array.data(), array.size())
	{}

	vector_view() = default;
};
