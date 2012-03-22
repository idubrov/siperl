{include, ["../include"]}.
{logdir, ["../logs"]}.
{suites, "apps/sip/test", [sip_ua_smoke_SUITE, sip_ua_CANCEL_SUITE, sip_ua_INVITE_SUITE]}.
{cover, "cover.spec"}.