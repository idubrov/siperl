{include, ["../include"]}.
{logdir, ["../logs"]}.
{suites, "apps/sip/test", [sip_transport_SUITE]}.
{cover, "cover.spec"}.