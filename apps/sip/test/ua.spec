{include, ["../include"]}.
{logdir, ["../logs"]}.
{suites, "apps/sip/test", [sip_simple_ua_SUITE]}.
{cover, "cover.spec"}.