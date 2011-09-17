{include, ["../include"]}.
{logdir, ["../logs"]}.
{suites, "apps/sip/test", [sip_ua_basic_SUITE]}.
{cover, "cover.spec"}.