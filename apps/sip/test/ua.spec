{include, ["../include"]}.
{logdir, ["../logs"]}.
{suites, "apps/sip/test", [sip_ua_basic_SUITE, sip_ua_CANCEL_SUITE]}.
{cover, "cover.spec"}.