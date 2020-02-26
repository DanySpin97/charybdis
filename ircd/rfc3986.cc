// Matrix Construct
//
// Copyright (C) Matrix Construct Developers, Authors & Contributors
// Copyright (C) 2016-2018 Jason Volk <jason@zemos.net>
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice is present in all copies. The
// full license for this software is available in the LICENSE file.

namespace ircd { namespace rfc3986
__attribute__((visibility("hidden")))
{
	using namespace ircd::spirit;

    struct encoder extern const encoder;
    struct decoder extern const decoder;
}}

namespace ircd { namespace rfc3986 { namespace parser
__attribute__((visibility("hidden")))
{
	using namespace ircd::spirit;
}}}

struct ircd::rfc3986::encoder
:karma::grammar<char *, const string_view>
{
	[[noreturn]] void throw_illegal()
	{
		throw encoding_error
		{
			"Generator Protection: urlencode"
		};
	}

	karma::rule<char *, const string_view> url_encoding
	{
		*(karma::char_("A-Za-z0-9") | (karma::lit('%') << karma::hex))
		,"url encoding"
	};

	encoder(): encoder::base_type{url_encoding} {}
}
const ircd::rfc3986::encoder;

struct ircd::rfc3986::decoder
:qi::grammar<const char *, mutable_buffer>
{
	template<class R = unused_type, class... S> using rule = qi::rule<const char *, R, S...>;

	rule<> url_illegal
	{
		char_(0x00, 0x1f)
		,"url illegal"
	};

	rule<char()> url_encodable
	{
		char_("A-Za-z0-9")
		,"url encodable character"
	};

	rule<char()> urlencoded_character
	{
		'%' > qi::uint_parser<char, 16, 2, 2>{}
		,"urlencoded character"
	};

	rule<mutable_buffer> url_decode
	{
		*((char_ - '%') | urlencoded_character)
		,"urldecode"
	};

	decoder(): decoder::base_type { url_decode } {}
}
const ircd::rfc3986::decoder;

decltype(ircd::rfc3986::parser::sub_delims)
ircd::rfc3986::parser::sub_delims
{
	lit('!') | lit('$') | lit('&') | lit('\'') |
	lit('(') | lit(')') | lit('*') | lit('+') |
	lit(',') | lit(';') | lit('=')
	,"sub-delims"
};

decltype(ircd::rfc3986::parser::gen_delims)
ircd::rfc3986::parser::gen_delims
{
	lit(':') | lit('/') | lit('?') | lit('#') |
	lit('[') | lit(']') | lit('@')
	,"gen-delims"
};

decltype(ircd::rfc3986::parser::unreserved)
ircd::rfc3986::parser::unreserved
{
	ascii::alpha | ascii::digit |
	lit('-') | lit('.') | lit('_') | lit('~')
	,"reserved"
};

decltype(ircd::rfc3986::parser::reserved)
ircd::rfc3986::parser::reserved
{
	gen_delims | sub_delims
	,"reserved"
};

decltype(ircd::rfc3986::parser::pct_encoded)
ircd::rfc3986::parser::pct_encoded
{
	lit('%') >> repeat(2)[ascii::xdigit]
	,"pct-encoded"
};

decltype(ircd::rfc3986::parser::pchar)
ircd::rfc3986::parser::pchar
{
	unreserved | pct_encoded | sub_delims | lit(':') | lit('@')
	,"pchar"
};

decltype(ircd::rfc3986::parser::query)
ircd::rfc3986::parser::query
{
	*(pchar | lit('/') | lit('?'))
	,"query"
};

decltype(ircd::rfc3986::parser::fragment)
ircd::rfc3986::parser::fragment
{
	*(pchar | lit('/') | lit('?'))
	,"fragment"
};

decltype(ircd::rfc3986::parser::segment_nz_nc)
ircd::rfc3986::parser::segment_nz_nc
{
	+(unreserved | pct_encoded | sub_delims | lit('@'))
	,"segment-nz-nc"
};

decltype(ircd::rfc3986::parser::segment_nz)
ircd::rfc3986::parser::segment_nz
{
	+pchar
	,"segment-nz"
};

decltype(ircd::rfc3986::parser::segment)
ircd::rfc3986::parser::segment
{
	*pchar
	,"segment"
};

decltype(ircd::rfc3986::parser::path_abempty)
ircd::rfc3986::parser::path_abempty
{
	*(lit('/') >> segment)
	,"path-abempty"
};

decltype(ircd::rfc3986::parser::path_noscheme)
ircd::rfc3986::parser::path_noscheme
{
	segment_nz_nc >> *(lit('/') >> segment)
	,"path-noscheme"
};

decltype(ircd::rfc3986::parser::path_rootless)
ircd::rfc3986::parser::path_rootless
{
	segment_nz >> *(lit('/') >> segment)
	,"path-rootless"
};

decltype(ircd::rfc3986::parser::path_absolute)
ircd::rfc3986::parser::path_absolute
{
	lit('/') >> -(path_rootless)
	,"path-absolute"
};

decltype(ircd::rfc3986::parser::path)
ircd::rfc3986::parser::path
{
	-(path_abempty | path_absolute | path_noscheme | path_rootless)
	,"path"
};

decltype(ircd::rfc3986::parser::reg_name)
ircd::rfc3986::parser::reg_name
{
	*(unreserved | pct_encoded | sub_delims)
	,"reg-name"
};

decltype(ircd::rfc3986::parser::userinfo)
ircd::rfc3986::parser::userinfo
{
	*(unreserved | pct_encoded | sub_delims | lit(':'))
	,"userinfo"
};

decltype(ircd::rfc3986::parser::port)
ircd::rfc3986::parser::port
{
	ushort_
	,"port number"
};

decltype(ircd::rfc3986::parser::ip4_octet)
ircd::rfc3986::parser::ip4_octet
{
	repeat(1,3)[char_("0-9")]
	,"IPv4 octet"
};

decltype(ircd::rfc3986::parser::ip4_address)
ircd::rfc3986::parser::ip4_address
{
	repeat(3)[ip4_octet >> '.'] >> ip4_octet
	,"IPv4 address"
};

decltype(ircd::rfc3986::parser::ip4_literal)
ircd::rfc3986::parser::ip4_literal
{
	ip4_address
	,"IPv4 literal"
};

decltype(ircd::rfc3986::parser::ip4_remote)
ircd::rfc3986::parser::ip4_remote
{
	ip4_literal >> -(':' > port)
	,"IPv4 remote"
};

decltype(ircd::rfc3986::parser::ip6_char)
ircd::rfc3986::parser::ip6_char
{
	char_("0-9a-fA-F")
	,"IPv6 character"
};

decltype(ircd::rfc3986::parser::ip6_h16)
ircd::rfc3986::parser::ip6_h16
{
	repeat(1,4)[ip6_char]
	,"IPv6 hexdigit"
};

decltype(ircd::rfc3986::parser::ip6_piece)
ircd::rfc3986::parser::ip6_piece
{
	ip6_h16 >> ':'
	,"IPv6 address piece"
};

// This is reversed from the BNF in the RFC otherwise it requires
// backtracking during the repeat[]; parsers are adjusted accordingly.
decltype(ircd::rfc3986::parser::ip6_ipiece)
ircd::rfc3986::parser::ip6_ipiece
{
	':' >> ip6_h16
	,"IPv6 address piece"
};

decltype(ircd::rfc3986::parser::ip6_ls32)
ircd::rfc3986::parser::ip6_ls32
{
	(ip6_h16 >> ':' >> ip6_h16) | ip4_address
};

/// https://tools.ietf.org/html/rfc3986 Appendix A
decltype(ircd::rfc3986::parser::ip6_addr)
ircd::rfc3986::parser::ip6_addr
{
	{                                                     repeat(6)[ip6_piece] >> ip6_ls32     },
	{                                        lit("::") >> repeat(5)[ip6_piece] >> ip6_ls32     },
	{                             ip6_h16 >> lit("::") >> repeat(4)[ip6_piece] >> ip6_ls32     },
	{  ip6_h16 >> repeat(0,1)[ip6_ipiece] >> lit("::") >> repeat(3)[ip6_piece] >> ip6_ls32     },
	{  ip6_h16 >> repeat(0,2)[ip6_ipiece] >> lit("::") >> repeat(2)[ip6_piece] >> ip6_ls32     },
	{  ip6_h16 >> repeat(0,3)[ip6_ipiece] >> lit("::") >> ip6_piece >> ip6_ls32                },
	{  ip6_h16 >> repeat(0,4)[ip6_ipiece] >> lit("::") >> ip6_ls32                             },
	{  ip6_h16 >> repeat(0,5)[ip6_ipiece] >> lit("::") >> -ip6_h16                             },
	{                                        lit("::") >> -ip6_h16                             },
};

decltype(ircd::rfc3986::parser::ip6_address)
ircd::rfc3986::parser::ip6_address
{
	ip6_addr[0] | ip6_addr[1] | ip6_addr[2] |
	ip6_addr[3] | ip6_addr[4] | ip6_addr[5] |
	ip6_addr[6] | ip6_addr[7] | ip6_addr[8]
	,"IPv6 address"
};

decltype(ircd::rfc3986::parser::ip6_literal)
ircd::rfc3986::parser::ip6_literal
{
	'[' >> ip6_address >> ']'
	,"IPv6 literal"
};

decltype(ircd::rfc3986::parser::ip6_remote)
ircd::rfc3986::parser::ip6_remote
{
	ip6_literal >> -(':' > port)
	,"IPv6 literal"
};

decltype(ircd::rfc3986::parser::ip_address)
ircd::rfc3986::parser::ip_address
{
	ip4_address | ip6_address
	,"IP address"
};

decltype(ircd::rfc3986::parser::ip_literal)
ircd::rfc3986::parser::ip_literal
{
	ip6_literal | ip4_literal
	,"IP literal"
};

decltype(ircd::rfc3986::parser::ip_remote)
ircd::rfc3986::parser::ip_remote
{
	ip_literal >> -(':' > port)
	,"IP literal"
};

decltype(ircd::rfc3986::parser::hostname)
ircd::rfc3986::parser::hostname
{
	char_("A-Za-z0-9") >> *(char_("A-Za-z0-9\x2D")) // x2D is '-'
	,"hostname"
};

decltype(ircd::rfc3986::parser::domain)
ircd::rfc3986::parser::domain
{
	hostname % '.'
	,"domain"
};

decltype(ircd::rfc3986::parser::hostport)
ircd::rfc3986::parser::hostport
{
	domain >> -(':' > port)
	,"hostport"
};

decltype(ircd::rfc3986::parser::host)
ircd::rfc3986::parser::host
{
	ip_address | domain
	,"host"
};

decltype(ircd::rfc3986::parser::host_literal)
ircd::rfc3986::parser::host_literal
{
	ip_literal | domain
	,"host literal"
};

decltype(ircd::rfc3986::parser::remote)
ircd::rfc3986::parser::remote
{
	ip_remote | hostport
	,"remote"
};

decltype(ircd::rfc3986::parser::authority)
ircd::rfc3986::parser::authority
{
	-(userinfo >> lit('@')) >> remote
	,"authority"
};

decltype(ircd::rfc3986::parser::scheme)
ircd::rfc3986::parser::scheme
{
	ascii::alpha >> *(ascii::alnum | lit('+') | lit('-') | lit('.'))
	,"scheme"
};

decltype(ircd::rfc3986::parser::hier_part)
ircd::rfc3986::parser::hier_part
{
	-((lit("//") >> authority >> path_abempty) | path_absolute | path_rootless)
	,"hier_part"
};

decltype(ircd::rfc3986::parser::relative_part)
ircd::rfc3986::parser::relative_part
{
	-((lit("//") >> authority >> path_abempty) | path_absolute | path_noscheme)
	,"relative-part"
};

decltype(ircd::rfc3986::parser::relative_ref)
ircd::rfc3986::parser::relative_ref
{
	relative_part >> -(lit('?') >> query) >> -(lit('#') >> fragment)
	,"relative-ref"
};

decltype(ircd::rfc3986::parser::absolute_uri)
ircd::rfc3986::parser::absolute_uri
{
	scheme >> lit(':') >> hier_part >> -(lit('?') >> query)
	,"absolute-URI"
};

decltype(ircd::rfc3986::parser::uri)
ircd::rfc3986::parser::uri
{
	scheme >> lit(':') >> hier_part >> -(lit('?') >> query) >> -(lit('#') >> fragment)
	,"URI"
};

decltype(ircd::rfc3986::parser::uri_ref)
ircd::rfc3986::parser::uri_ref
{
	uri | relative_ref
	,"URI-reference"
};

//
// uri decompose
//

BOOST_FUSION_ADAPT_STRUCT
(
    ircd::rfc3986::uri,
    ( decltype(ircd::rfc3986::uri::scheme),    scheme    )
    ( decltype(ircd::rfc3986::uri::user),      user      )
    ( decltype(ircd::rfc3986::uri::remote),    remote    )
    ( decltype(ircd::rfc3986::uri::path),      path      )
    ( decltype(ircd::rfc3986::uri::query),     query     )
    ( decltype(ircd::rfc3986::uri::fragment),  fragment  )
)

ircd::rfc3986::uri::uri(const string_view &input)
{
	static const parser::rule<rfc3986::uri> rule
	{
		raw[parser::scheme] >> lit("://")
		>> -raw[parser::userinfo >> lit('@')]
		>> raw[parser::remote]
		>> raw[parser::path_abempty]
		>> -raw[lit('?') >> parser::query]
		>> -raw[lit('#') >> parser::fragment]
	};

	const char *start(begin(input)), *const stop(end(input));
	qi::parse(start, stop, eps > rule, *this);

	//TODO: XXX Can this go?
	this->user = rstrip(this->user, '@');
	this->query = lstrip(this->query, '?');
	this->fragment = lstrip(this->fragment, '#');
}

//
// general interface
//

ircd::string_view
ircd::rfc3986::encode(const mutable_buffer &out,
                      const json::members &members)
{
	window_buffer buf{out};
	const auto append{[&buf](const json::member &member)
	{
		assert(type(member.first) == json::STRING);
		if(unlikely(!member.second.serial && type(member.second) != json::STRING))
			throw panic
			{
				"Cannot encode non-serial json::member type '%s'",
				reflect(type(member.second))
			};

		consume(buf, size(encode(buf, member.first)));
		consume(buf, copy(buf, "="_sv));
		consume(buf, size(encode(buf, member.second)));
	}};

	auto it(begin(members));
	if(it != end(members))
	{
		append(*it);
		for(++it; it != end(members); ++it)
		{
			consume(buf, copy(buf, "&"_sv));
			append(*it);
		}
	}

	return buf.completed();
}

ircd::string_view
ircd::rfc3986::encode(const mutable_buffer &buf,
                      const string_view &url)
{
	char *out(data(buf));
	karma::generate(out, maxwidth(size(buf))[encoder], url);
	return string_view
	{
		data(buf), size_t(std::distance(data(buf), out))
	};
}

ircd::string_view
ircd::rfc3986::decode(const mutable_buffer &buf,
                      const string_view &url)
try
{
	const char *start(url.data()), *const stop
	{
		start + std::min(size(url), size(buf))
	};

	mutable_buffer mb
	{
		data(buf), size_t(0)
	};

	qi::parse(start, stop, eps > decoder, mb);
	return string_view
	{
		data(mb), size(mb)
	};
}
catch(const qi::expectation_failure<const char *> &e)
{
	const auto rule
	{
		ircd::string(e.what_)
	};

	throw decoding_error
	{
		"I require a valid urlencoded %s. You sent %zu invalid chars starting with `%s'.",
		between(rule, "<", ">"),
		size_t(e.last - e.first),
		string_view{e.first, e.last}
	};
}

ircd::string_view
ircd::rfc3986::host(const string_view &str)
try
{
	static const parser::rule<string_view> literal
	{
		parser::ip6_address
	};

	static const parser::rule<string_view> non_literal
	{
		parser::ip6_address | parser::ip4_address | parser::domain
	};

	static const parser::rule<string_view> rule
	{
		(lit('[') >> raw[literal]) | raw[non_literal]
		,"host"
	};

	string_view ret;
	const char *start(str.data()), *const stop(start + str.size());
	qi::parse(start, stop, eps > rule, ret);
	return ret;
}
catch(const qi::expectation_failure<const char *> &e)
{
	throw expectation_failure<error>{e};
}

uint16_t
ircd::rfc3986::port(const string_view &str)
try
{
	static const parser::rule<> literal
	{
		parser::ip6_literal | parser::ip4_literal | parser::domain
	};

	static const parser::rule<> non_literal
	{
		parser::ip6_address
	};

	static const parser::rule<uint16_t> rule
	{
		non_literal | (literal >> -(lit(':') >> parser::port) >> eoi)
		,"port"
	};

	uint16_t ret(0);
	const char *start(str.data()), *const stop(start + str.size());
	qi::parse(start, stop, rule, ret);
	return ret;
}
catch(const qi::expectation_failure<const char *> &e)
{
	throw expectation_failure<error>{e};
}

//
// validators
//

bool
ircd::rfc3986::valid_remote(std::nothrow_t,
                            const string_view &str)
{
	if(str.size() > DOMAIN_MAX + 6)
		return false;

	return valid(std::nothrow, parser::remote, str);
}

void
ircd::rfc3986::valid_remote(const string_view &str)
{
	if(str.size() > DOMAIN_MAX + 6)
		throw error
		{
			"String length %zu exceeds maximum of %zu characters",
			size(str),
			DOMAIN_MAX + 6
		};

	valid(parser::remote, str);
}

bool
ircd::rfc3986::valid_domain(std::nothrow_t,
                           const string_view &str)
{
	if(str.size() > DOMAIN_MAX)
		return false;

	return valid(std::nothrow, parser::domain, str);
}

void
ircd::rfc3986::valid_domain(const string_view &str)
{
	if(str.size() > DOMAIN_MAX)
		throw error
		{
			"String length %zu exceeds maximum of %zu characters",
			size(str),
			DOMAIN_MAX
		};

	valid(parser::domain, str);
}

bool
ircd::rfc3986::valid_host(std::nothrow_t,
                          const string_view &str)
{
	if(str.size() > DOMAIN_MAX)
		return false;

	return valid(std::nothrow, parser::host, str);
}

void
ircd::rfc3986::valid_host(const string_view &str)
{
	if(str.size() > DOMAIN_MAX)
		throw error
		{
			"String length %zu exceeds maximum of %zu characters",
			size(str),
			DOMAIN_MAX
		};

	valid(parser::host, str);
}

bool
ircd::rfc3986::valid_hostname(std::nothrow_t,
                              const string_view &str)
{
	if(str.size() > HOSTNAME_MAX)
		return false;

	return valid(std::nothrow, parser::hostname, str);
}

void
ircd::rfc3986::valid_hostname(const string_view &str)
{
	if(str.size() > HOSTNAME_MAX)
		throw error
		{
			"String length %zu exceeds maximum of %zu characters",
			size(str),
			HOSTNAME_MAX
		};

	valid(parser::hostname, str);
}

bool
ircd::rfc3986::valid(std::nothrow_t,
                     const parser::rule<> &rule,
                     const string_view &str)
{
	const parser::rule<> only_rule
	{
		rule >> eoi
	};

	const char *start(str.data()), *const stop(start + str.size());
	return qi::parse(start, stop, only_rule);
}

void
ircd::rfc3986::valid(const parser::rule<> &rule,
                     const string_view &str)
try
{
	const parser::rule<> only_rule
	{
		rule >> eoi
	};

	const char *start(str.data()), *const stop(start + str.size());
	qi::parse(start, stop, eps > only_rule);
}
catch(const qi::expectation_failure<const char *> &e)
{
	throw expectation_failure<error>{e};
}
