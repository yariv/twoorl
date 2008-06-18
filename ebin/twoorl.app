{application, twoorl, [
  {description, "Twoorl is an open-source Twitter clone."},
  {vsn, "0.3"},
  {modules, [twoorl, twoorl_server, twoorl_sup]},
  {registered, [twoorl]},
  {applications, [kernel, stdlib, sasl, crypto, inets, mnesia]},
  {mod, {twoorl, []}},
  {env, [
    {dbconns, [
        {"localhost", "root", "password", "twoorl", 3}
    ]},
    {tables, [session]}
  ]},
  {start_phases, [
    {mysql, []},
    {mnesia, []},
    {compile, []}
  ]}
]}.
