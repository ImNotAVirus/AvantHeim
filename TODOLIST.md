# TODOLIST

## Views

Move all Views in ChannelService into ElvenViews

## Simple ECS pattern

For example:

```elixir
protocol Entity
- Entity.type()
- Entity.id()

protocol MovableEntity
- Entity.position()
- Entity.position(new_position)
```

## PresenceManager

Move PresenceManager inside is own app?  
To clean cache when I restart the ChannelService
