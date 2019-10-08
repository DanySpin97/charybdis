// Matrix Construct
//
// Copyright (C) Matrix Construct Developers, Authors & Contributors
// Copyright (C) 2016-2018 Jason Volk <jason@zemos.net>
//
// Permission to use, copy, modify, and/or distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice is present in all copies. The
// full license for this software is available in the LICENSE file.

using namespace ircd;

mapi::header
IRCD_MODULE
{
	"Web hook Handler"
};

conf::item<std::string>
webhook_secret
{
	{ "name", "webhook.secret" }
};

conf::item<std::string>
webhook_user
{
	{ "name", "webhook.user" }
};

conf::item<std::string>
webhook_room
{
	{ "name", "webhook.room" }
};

conf::item<std::string>
webhook_url
{
	{ "name", "webhook.url" },
	{ "default", "/webhook" }
};

conf::item<bool>
webhook_status_verbose
{
	{ "name",     "webhook.github.status.verbose" },
	{ "default",  true                            },
};

resource
webhook_resource
{
	string_view{webhook_url},
	{
		"Webhook Resource",
		webhook_resource.DIRECTORY
	}
};

static resource::response
post__webhook(client &,
              const resource::request &);

resource::method
webhook_post
{
	webhook_resource, "POST", post__webhook
};

static void
github_handle(client &,
              const resource::request &);

static void
appveyor_handle(client &,
                const resource::request &);

resource::response
post__webhook(client &client,
              const resource::request &request)
{
	if(has(http::headers(request.head.headers), "X-GitHub-Event"_sv))
		github_handle(client, request);

	else if(has(http::headers(request.head.headers), "X-Appveyor-Secret"_sv))
		appveyor_handle(client, request);

	return resource::response
	{
		client, http::OK
	};
}

static bool
github_validate(const string_view &sig,
                const const_buffer &content,
                const string_view &secret);

static std::string
github_url(const json::string &url);

static json::string
github_find_commit_hash(const json::object &content);

static json::string
github_find_issue_number(const json::object &content);

static std::pair<json::string, json::string>
github_find_party(const json::object &content);

static bool
github_handle__push(std::ostream &,
                    const json::object &content);

static bool
github_handle__pull_request(std::ostream &,
                            const json::object &content);

static bool
github_handle__issue_comment(std::ostream &,
                             const json::object &content);

static bool
github_handle__issues(std::ostream &,
                      const json::object &content);

static bool
github_handle__watch(std::ostream &,
                     const json::object &content);

static bool
github_handle__star(std::ostream &,
                    const json::object &content);

static bool
github_handle__label(std::ostream &,
                     const json::object &content);

static bool
github_handle__organization(std::ostream &,
                            const json::object &content);

static bool
github_handle__status(std::ostream &,
                      const json::object &content);

static bool
github_handle__repository(std::ostream &,
                          const json::object &content);

static bool
github_handle__delete(std::ostream &,
                      const json::object &content);

static bool
github_handle__create(std::ostream &,
                      const json::object &content);

static bool
github_handle__ping(std::ostream &,
                    const json::object &content);

static std::ostream &
github_heading(std::ostream &,
               const string_view &type,
               const json::object &content);

void
github_handle(client &client,
              const resource::request &request)
{
	if(!string_view(webhook_room))
		return;

	if(!string_view(webhook_user))
		return;

	const http::headers &headers
	{
		request.head.headers
	};

	const auto sig
	{
		headers.at("X-Hub-Signature")
	};

	if(!github_validate(sig, request.content, webhook_secret))
		throw http::error
		{
			http::UNAUTHORIZED, "X-Hub-Signature verification failed"
		};

	const string_view &type
	{
		headers.at("X-GitHub-Event")
	};

	const string_view &delivery
	{
		headers.at("X-GitHub-Delivery")
	};

	const unique_buffer<mutable_buffer> buf
	{
		48_KiB
	};

	std::stringstream out;
	pubsetbuf(out, buf);

	github_heading(out, type, request.content);

	const bool ok
	{
		type == "ping"?
			github_handle__ping(out, request.content):
		type == "push"?
			github_handle__push(out, request.content):
		type == "pull_request"?
			github_handle__pull_request(out, request.content):
		type == "issues"?
			github_handle__issues(out, request.content):
		type == "issue_comment"?
			github_handle__issue_comment(out, request.content):
		type == "watch"?
			github_handle__watch(out, request.content):
		type == "star"?
			github_handle__star(out, request.content):
		type == "label"?
			github_handle__label(out, request.content):
		type == "organization"?
			github_handle__organization(out, request.content):
		type == "status"?
			github_handle__status(out, request.content):
		type == "repository"?
			github_handle__repository(out, request.content):
		type == "create"?
			github_handle__create(out, request.content):
		type == "delete"?
			github_handle__delete(out, request.content):

		true // unhandled will just show heading
	};

	if(!ok)
		return;

	const auto room_id
	{
		m::room_id(string_view(webhook_room))
	};

	const m::user::id::buf user_id
	{
		string_view(webhook_user), my_host()
	};

	const auto evid
	{
		m::msghtml(room_id, user_id, view(out, buf), "No alt text", "m.notice")
	};

	log::info
	{
		"Webhook [%s] '%s' delivered to %s %s",
		delivery,
		type,
		string_view{room_id},
		string_view{evid}
	};
}

static std::ostream &
github_heading(std::ostream &out,
               const string_view &type,
               const json::object &content)
{
	const json::object repository
	{
		content["repository"]
	};

	const json::object organization
	{
		content["organization"]
	};

	if(empty(repository))
	{
		const auto url
		{
			github_url(organization["url"])
		};

		out << "<a href=\"" << url << "\">"
		    << json::string(organization["login"])
		    << "</a>";
	}
	else
		out << "<a href=" << repository["html_url"] << ">"
		    << json::string(repository["full_name"])
		    << "</a>";

	const auto commit_hash
	{
		github_find_commit_hash(content)
	};

	if(commit_hash && type == "push")
		out << " <b><font color=\"#FF5733\">";
	else if(commit_hash && type == "pull_request")
		out << " <b><font color=\"#FF5733\">";
	else if(commit_hash)
		out << " <b>";

	if(commit_hash)
		out << commit_hash.substr(0, 8)
		    << "</font></b>";

	const string_view issue_number
	{
		github_find_issue_number(content)
	};

	if(issue_number)
		out << " <b>#" << issue_number << "</b>";
	else
		out << " " << type;

	const auto party
	{
		github_find_party(content)
	};

	out << " by "
	    << "<a href=\""
	    << party.second
	    << "\">"
	    << party.first
	    << "</a>";

	return out;
}

bool
github_handle__push(std::ostream &out,
                    const json::object &content)
{
	const json::array commits
	{
		content["commits"]
	};

	const auto count
	{
		size(commits)
	};

	if(!count)
	{
		out << " <font color=\"#FF0000\">";
		if(content["ref"])
			out << " " << json::string(content["ref"]);

		out << " deleted</font>";
		return true;
	}

	if(content["ref"])
	{
		const json::string ref(content["ref"]);
		out << " "
		    << " "
		    << token_last(ref, '/');
	}

	out << " <a href=\"" << json::string(content["compare"]) << "\">"
	    << "<b>" << count << " commits</b>"
	    << "</a>";

	if(content["forced"] == "true")
		out << " (rebase)";

	out << "<pre><code>";
	for(ssize_t i(count - 1); i >= 0; --i)
	{
		const json::object &commit(commits.at(i));
		const json::string url(commit["url"]);
		const json::string id(commit["id"]);
		const auto sid(id.substr(0, 8));
		out << " <a href=\"" << url << "\">"
		    << "<b>" << sid << "</b>"
		    << "</a>";

		const json::object author(commit["author"]);
		out << " <b>"
		    << json::string(author["name"])
		    << "</b>"
		    ;

		const json::object committer(commit["committer"]);
		if(committer["email"] != author["email"])
			out << " via <b>"
			    << committer["name"]
			    << "</b>"
			    ;

		const json::string message(commit["message"]);
		const auto summary
		{
			split(message, "\\n").first
		};

		out << " "
		    << summary
		    ;

		out << "<br />";
	}

	out << "</code></pre>";
	return true;
}

bool
github_handle__pull_request(std::ostream &out,
                            const json::object &content)
{
	const json::object pr
	{
		content["pull_request"]
	};

	if(pr["merged"] != "true")
		out << " "
		    << "<b>"
		    << json::string(content["action"])
		    << "</b>"
		    ;

	if(pr["title"])
		out << " "
		    << "<a href="
		    << pr["html_url"]
		    << ">"
		    << json::string(pr["title"])
		    << "</a>"
		    ;

	const json::object head
	{
		pr["head"]
	};

	const json::object base
	{
		pr["base"]
	};

	for(const json::object &label : json::array(pr["labels"]))
	{
		out << "&nbsp;";
		out << "<font color=\"#FFFFFF\""
		    << "data-mx-bg-color=\"#"
		    << json::string(label["color"])
		    << "\">";

		out << "<b>";
		out << "&nbsp;";
			out << json::string(label["name"]);
		out << "&nbsp;";
		out << "</b>";

		out << "</font>";
	}

	if(pr["merged"] == "true")
		out << ' '
		    << "<font color=\"#FFFFFF\""
		    << "data-mx-bg-color=\"#6f42c1\">"
		    << "&nbsp;<b>"
		    << "merged"
		    << "</b>&nbsp;"
		    << "</font>"
		    ;

	if(pr.has("merged_by") && pr["merged_by"] != "null")
	{
		const json::object merged_by{pr["merged_by"]};
		out << " "
		    << "by "
		    << "<a href=\""
		    << json::string(merged_by["html_url"])
		    << "\">"
		    << json::string(merged_by["login"])
		    << "</a>"
		    ;
	}

	const json::string &body
	{
		pr["body"]
	};

	if(!empty(body))
		out << " "
		    << "<pre><code>"
		    << body
		    << "</code></pre>"
		    ;

	if(pr.has("commits"))
		out << " "
		    << "<b>"
		    << pr["commits"]
		    << ' '
		    << "<a href="
		    << github_url(pr["commits_url"])
		    << ">"
		    << "commits"
		    << "</a>"
		    << "</b>"
		    ;

	if(pr.has("comments"))
		out << " / "
		    << "<b>"
		    << pr["comments"]
		    << ' '
		    << "<a href="
		    << github_url(pr["comments_url"])
		    << ">"
		    << "comments"
		    << "</a>"
		    << "</b>"
		    ;

	if(pr.has("changed_files"))
		out << " / "
		    << "<b>"
		    << pr["changed_files"]
		    << ' '
		    << "<a href=\""
		    << json::string(pr["html_url"])
		    << "/files"
		    << "\">"
		    << "files"
		    << "</a>"
		    << "</b>"
		    ;

	if(pr.has("additions"))
		out << " / "
		    << "<b>"
		    << "<font color=\"#33CC33\">"
		    << "++"
		    << "</font>"
		    << pr["additions"]
		    << "</b>"
		    ;

	if(pr.has("deletions"))
		out << " / "
		    << "<b>"
		    << "<font color=\"#CC0000\">"
		    << "--"
		    << "</font>"
		    << pr["deletions"]
		    << "</b>"
		    ;

	if(pr["merged"] == "false") switch(hash(pr["mergeable"]))
	{
		default:
		case "null"_:
			out << " / "
			    << "<font color=\"#000000\""
			    << "data-mx-bg-color=\"#EEBB00\">"
			    << "<b>"
			    << "&nbsp;"
			    << "CHECKING"
			    << "&nbsp;"
			    << "</b>"
			    << "</font>"
			    ;
			break;

		case "true"_:
			out << " / "
			    << "<font color=\"#FFFFFF\""
			    << "data-mx-bg-color=\"#03B381\">"
			    << "<b>"
			    << "&nbsp;"
			    << "NO CONFLICTS"
			    << "&nbsp;"
			    << "</b>"
			    << "</font>"
			    ;
			break;

		case "false"_:
			out << " / "
			    << "<font color=\"#FFFFFF\""
			    << "data-mx-bg-color=\"#CC0000\">"
			    << "<b>"
			    << "&nbsp;"
			    << "MERGE CONFLICT"
			    << "&nbsp;"
			    << "</b>"
			    << "</font>"
			    ;
			break;
	}

	return true;
}

bool
github_handle__issues(std::ostream &out,
                      const json::object &content)
{
	const json::string &action
	{
		content["action"]
	};

	out << " "
	    << "<b>"
	    << action
	    << "</b>"
	    ;

	const json::object issue
	{
		content["issue"]
	};

	switch(hash(action))
	{
		case "assigned"_:
		case "unassigned"_:
		{
			const json::object assignee
			{
				content["assignee"]
			};

			out << " "
			    << "<a href=\""
			    << json::string(assignee["html_url"])
			    << "\">"
			    << json::string(assignee["login"])
			    << "</a>"
			    ;

			break;
		}
	}

	out << " "
	    << "<a href=\""
	    << json::string(issue["html_url"])
	    << "\">"
	    << "<b><u>"
	    << json::string(issue["title"])
	    << "</u></b>"
	    << "</a>"
	    ;

	for(const json::object &label : json::array(issue["labels"]))
	{
		out << "&nbsp;";
		out << "<font color=\"#FFFFFF\""
		    << "data-mx-bg-color=\"#"
		    << json::string(label["color"])
		    << "\">";

		out << "<b>";
		out << "&nbsp;";
			out << json::string(label["name"]);
		out << "&nbsp;";
		out << "</b>";

		out << "</font>";
	}

	if(action == "opened")
	{
		out << " "
		    << "<blockquote>"
		    << "<pre><code>"
		    ;

		static const auto delim("\\r\\n");
		const json::string body(issue["body"]);
		auto lines(split(body, delim)); do
		{
			out << lines.first
			    << "<br />"
			    ;

			lines = split(lines.second, delim);
		}
		while(!empty(lines.second));

		out << ""
		    << "</code></pre>"
		    << "</blockquote>"
		    ;
	}
	else if(action == "labeled")
	{
		// quiet these messages for now until we can figure out how to reduce
		// noise around issue opens.
		return false;

		const json::object label
		{
			content["label"]
		};

		out << "<ul>";
		out << "<li>added: ";
		out << "<font color=\"#FFFFFF\""
		    << "data-mx-bg-color=\"#"
		    << json::string(label["color"])
		    << "\">";

		out << "<b>";
		out << "&nbsp;";
		out << json::string(label["name"]);
		out << "&nbsp;";
		out << "</b>";

		out << "</font>";
		out << "</li>";
		out << "</ul>";
	}
	else if(action == "unlabeled")
	{
		// quiet these messages for now until we can figure out how to reduce
		// noise around issue opens.
		return false;

		const json::object label
		{
			content["label"]
		};

		out << "<ul>";
		out << "<li>removed: ";
		out << "<font color=\"#FFFFFF\""
		    << "data-mx-bg-color=\"#"
		    << json::string(label["color"])
		    << "\">";

		out << "<b>";
		out << "&nbsp;";
		out << json::string(label["name"]);
		out << "&nbsp;";
		out << "</b>";

		out << "</font>";
		out << "</li>";
		out << "</ul>";
	}

	return true;
}

bool
github_handle__issue_comment(std::ostream &out,
                             const json::object &content)
{
	const json::object issue
	{
		content["issue"]
	};

	const json::object comment
	{
		content["comment"]
	};

	const json::string action
	{
		content["action"]
	};

	out << " <b>";
	switch(hash(action))
	{
		case "created"_:
			out << "commented on";
			break;

		default:
			out << action;
			break;
	}
	out << "</b>";

	out << " "
	    << "<a href=\""
	    << json::string(issue["html_url"])
	    << "\">"
	    << "<b><u>"
	    << json::string(issue["title"])
	    << "</u></b>"
	    << "</a>"
	    ;

	if(action == "created")
	{
		out << " "
		    << "<blockquote>"
		    << "<pre><code>"
		    ;

		static const auto delim("\\r\\n");
		const json::string body(comment["body"]);
		auto lines(split(body, delim)); do
		{
			out << lines.first
			    << "<br />"
			    ;

			lines = split(lines.second, delim);
		}
		while(!empty(lines.second));

		out << ""
		    << "</code></pre>"
		    << "</blockquote>"
		    ;
	}

	return true;
}

bool
github_handle__label(std::ostream &out,
                     const json::object &content)
{
	const json::string &action
	{
		content["action"]
	};

	out << " "
	    << "<b>"
	    << action
	    << "</b>"
	    ;

	const json::object &label
	{
		content["label"]
	};

	out << "<ul>";
	out << "<li>";
	out << "<font color=\"#FFFFFF\""
	    << "data-mx-bg-color=\"#"
	    << json::string(label["color"])
	    << "\">";

	out << "<b>";
	out << " &nbsp; ";
	out << json::string(label["name"]);
	out << " &nbsp; ";
	out << "</b>";
	out << "</font>";
	out << "</li>";
	out << "</ul>";

	if(action == "edited")
	{
		const json::object &changes
		{
			content["changes"]
		};

		const json::object &color_obj
		{
			changes["color"]
		};

		const json::string &color
		{
			empty(color_obj)?
				label["color"]:
				color_obj["from"]
		};

		const json::object &name_obj
		{
			changes["name"]
		};

		const json::string &name
		{
			empty(name_obj)?
				label["name"]:
				name_obj["from"]
		};

		out << "from: ";
		out << "<ul>";
		out << "<li>";
		out << "<font color=\"#FFFFFF\""
		    << "data-mx-bg-color=\"#"
		    << color
		    << "\">";

		out << "<b>";
		out << " &nbsp; ";
		out << name;
		out << " &nbsp; ";
		out << "</b>";
		out << "</font>";
		out << "</li>";
		out << "</ul>";
	}

	return true;
}

bool
github_handle__organization(std::ostream &out,
                            const json::object &content)
{
	const json::string &action
	{
		content["action"]
	};

	const auto &action_words
	{
		split(action, '_')
	};

	out << " " << "<b>";

	if(action_words.second)
		out
		<< split(action, '_').second
		<< " ";

	out << action_words.first << "</b>";

	if(action == "member_added")
	{
		const json::object &membership
		{
			content["membership"]
		};

		const json::object &user
		{
			membership["user"]
		};

		out << " "
		    << "<a href=" << user["html_url"] << ">"
		    << json::string(user["login"])
		    << "</a>"
		    ;

		out << " with role "
		    << json::string(membership["role"])
		    ;
	}
	else if(action == "member_removed")
	{
		const json::object &membership
		{
			content["membership"]
		};

		const json::object &user
		{
			membership["user"]
		};

		out << " "
		    << "<a href=" << user["html_url"] << ">"
		    << json::string(user["login"])
		    << "</a>"
		    ;
	}
	else if(action == "member_invited")
	{
		const json::object &invitation
		{
			content["invitation"]
		};

		const json::object &user
		{
			invitation["user"]
		};

		out << " "
		    << "<a href=" << user["html_url"] << ">"
		    << json::string(user["login"])
		    << "</a>"
		    ;
	}

	return true;
}

bool
github_handle__status(std::ostream &out,
                      const json::object &content)
{
	const json::string &state
	{
		content["state"]
	};

	if(state == "pending")
		return false;

	if(!webhook_status_verbose) switch(hash(state))
	{
		case "failure"_:
			break;

		default:
			return false;
	}

	const json::string &description
	{
		content["description"]
	};

	const string_view &url
	{
		content["target_url"]
	};

	if(state == "success")
		out << " "
		    << "<font data-mx-bg-color=\"#03B381\">"
		    ;

	else if(state == "failure")
		out << " "
		    << "<font data-mx-bg-color=\"#CC0000\">"
		    ;

	else if(state == "error")
		out << " "
		    << "<font data-mx-bg-color=\"#280000\">"
		    ;

	out << "&nbsp;"
	    << "<a href="
	    << url
	    << ">";

	out << ""
	    << "<font color=\"#FFFFFF\">"
	    << "<b>"
	    << description
	    << "</b>"
	    << "</font>"
	    << "</a>"
	    << "&nbsp;"
	    << "</font>"
	    ;

	return true;
}

bool
github_handle__watch(std::ostream &out,
                     const json::object &content)
{
	const json::string &action
	{
		content["action"]
	};

	if(action != "started")
		return false;

	// There appears to be no way to distinguish between a genuine watch
	// button click and just a star; the watch event is sent for both.
	// Returning false just disables this event so there's no double-message.
	return false;
}

bool
github_handle__star(std::ostream &out,
                    const json::object &content)
{
	const json::string &action
	{
		content["action"]
	};

	if(action != "created")
		return false;

	return true;
}

bool
github_handle__repository(std::ostream &out,
                          const json::object &content)
{
	const json::string &action
	{
		content["action"]
	};

	out << ' ' << action;
	out << "<pre><code>"
	    << json::string(content["description"])
	    << "</code></pre>";

	return true;
}

bool
github_handle__create(std::ostream &out,
                      const json::object &content)
{
	const json::string &ref
	{
		content["ref"]
	};

	const json::string &ref_type
	{
		content["ref_type"]
	};

	out << ' '
	    << ref_type
	    << ' '
	    << ref
	    ;

	return true;
}

bool
github_handle__delete(std::ostream &out,
                      const json::object &content)
{
	const json::string &ref
	{
		content["ref"]
	};

	const json::string &ref_type
	{
		content["ref_type"]
	};

	out << ' '
	    << ref_type
	    << ' '
	    << ref
	    ;

	return true;
}

bool
github_handle__ping(std::ostream &out,
                    const json::object &content)
{
	out << "<pre><code>"
	    << json::string(content["zen"])
	    << "</code></pre>";

	return true;
}

/// Researched from yestifico bot
std::pair<json::string, json::string>
github_find_party(const json::object &content)
{
	const json::object pull_request
	{
		content["pull_request"]
	};

	const json::object user
	{
		pull_request["user"]
	};

	if(!empty(user))
		return
		{
			user["login"], user["html_url"]
		};

	const json::object sender
	{
		content["sender"]
	};

	return
	{
		sender["login"], sender["html_url"]
	};
}

/// Researched from yestifico bot
json::string
github_find_issue_number(const json::object &content)
{
	const json::object issue(content["issue"]);
	if(!empty(issue))
		return issue["number"];

	if(content["number"])
		return content["number"];

	return {};
}

/// Researched from yestifico bot
json::string
github_find_commit_hash(const json::object &content)
{
	if(content["sha"])
		return content["sha"];

	const json::object commit(content["commit"]);
	if(!empty(commit))
		return commit["sha"];

	const json::object head(content["head"]);
	if(!empty(head))
		return head["commit"];

	const json::object head_commit(content["head_commit"]);
	if(!empty(head_commit))
		return head_commit["id"];

	const json::object comment(content["comment"]);
	if(!empty(comment))
		return comment["commit_id"];

	if(content["commit"])
		return content["commit"];

	const json::object pr{content["pull_request"]};
	const json::object prhead{pr["head"]};
	if(prhead["sha"])
		return prhead["sha"];

	return {};
}

std::string
github_url(const json::string &url)
{
	std::string base("https://");
	return base + std::string(lstrip(url, "https://api."));
}

bool
github_validate(const string_view &sigheader,
                const const_buffer &content,
                const string_view &secret)
try
{
	const auto sig
	{
		split(sigheader, "=")
	};

	crh::hmac hmac
	{
		sig.first, secret
	};

	hmac.update(content);

	char ubuf[64], abuf[sizeof(ubuf) * 2];
	if(unlikely(sizeof(ubuf) < hmac.length()))
		throw ircd::panic
		{
			"HMAC algorithm '%s' digest exceeds buffer size.",
			sig.first
		};

	const const_buffer hmac_bin
	{
		hmac.finalize(ubuf)
	};

	static_assert(sizeof(abuf) >= sizeof(ubuf) * 2);
	const string_view hmac_hex
	{
		u2a(abuf, hmac_bin)
	};

	return hmac_hex == sig.second;
}
catch(const crh::error &e)
{
	throw http::error
	{
		http::NOT_IMPLEMENTED, "The signature algorithm is not supported.",
	};
}

void
appveyor_handle(client &client,
                const resource::request &request)
{
	const http::headers &headers
	{
		request.head.headers
	};
}
