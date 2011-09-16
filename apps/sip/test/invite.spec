{include, ["../include"]}.
{logdir, ["../logs"]}.
{suites, "apps/sip/test", [sip_invite_SUITE]}.
{cover, "cover.spec"}.