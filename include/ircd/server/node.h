// Copyright (C) Matrix Construct Developers, Authors & Contributors
// Copyright (C) 2016-2018 Jason Volk
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice is present in all copies.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
// INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
// STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
// IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

#pragma once
#define HAVE_IRCD_SERVER_NODE_H

/// Remote entity.
///
struct ircd::server::node
:std::enable_shared_from_this<ircd::server::node>
{
	net::remote remote;
	std::exception_ptr eptr;
	std::list<link> links;

	template<class F> size_t accumulate_links(F&&) const;
	template<class F> size_t accumulate_tags(F&&) const;

	void handle_resolve(std::weak_ptr<node>, std::exception_ptr, const ipport &);
	void resolve(const hostport &);

	void disperse_uncommitted(link &);
	void cancel_committed(link &, std::exception_ptr);
	void disperse(link &);
	void del(link &);

	void handle_link_done(link &);
	void handle_tag_done(link &, tag &) noexcept;
	void handle_error(link &, const boost::system::system_error &);
	void handle_error(link &, std::exception_ptr);
	void handle_close(link &, std::exception_ptr);
	void handle_open(link &, std::exception_ptr);

  public:
	// config related
	size_t link_max() const;
	size_t link_min() const;

	// stats for all links in node
	size_t link_count() const;
	size_t link_busy() const;
	size_t link_ready() const;

	// stats for all tags in all links in node
	size_t tag_count() const;
	size_t tag_committed() const;
	size_t tag_uncommitted() const;

	// stats for all upload-side bytes in all tags in all links
	size_t write_total() const;
	size_t write_completed() const;
	size_t write_remaining() const;

	// stats for download-side bytes in all tags in all links (note:
	// see notes in link.h/tag.h about inaccuracy here).
	size_t read_total() const;
	size_t read_completed() const;
	size_t read_remaining() const;

	// link control panel
	link &link_add(const size_t &num = 1);
	link *link_get(const request &);

	// request panel
	void submit(request &);

	// control panel
	void interrupt();
	void close();

	node();
	~node() noexcept;
};
