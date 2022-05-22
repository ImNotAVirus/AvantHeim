# Currently ELixirLS doesn't works with VSCode workspace.
# So I'll just define formatter settings here

# Used by "mix format"
[
  locals_without_parens: [
    # elven_core
    ignore_packet: 1,
    packet: 2,
    field: 2,
    field: 3,
    resolve: 2,
    defcommand: 2,

    # elven_enums
    test_enum: 2,

    # elven_views
    defpacket: 1,
    defpacket: 2,
    field: 2,
    field: 3,

    # simple_enum
    defenum: 2,

    # ecto
    from: 2,
    field: 1,
    field: 2,
    field: 3,
    timestamps: 1,
    belongs_to: 2,
    belongs_to: 3,
    has_one: 2,
    has_one: 3,
    has_many: 2,
    has_many: 3,
    many_to_many: 2,
    many_to_many: 3,
    embeds_one: 2,
    embeds_one: 3,
    embeds_one: 4,
    embeds_many: 2,
    embeds_many: 3,
    embeds_many: 4,

    # ecto_sql
    add: 2,
    add: 3,
    add_if_not_exists: 2,
    add_if_not_exists: 3,
    alter: 2,
    create: 1,
    create: 2,
    create_if_not_exists: 1,
    create_if_not_exists: 2,
    drop: 1,
    drop: 2,
    drop_if_exists: 1,
    drop_if_exists: 2,
    execute: 1,
    execute: 2,
    modify: 2,
    modify: 3,
    remove: 1,
    remove: 2,
    remove: 3,
    remove_if_exists: 2,
    rename: 2,
    rename: 3,
    timestamps: 1
  ]
]
