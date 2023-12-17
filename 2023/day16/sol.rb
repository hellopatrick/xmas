#!/usr/bin/env ruby
# frozen_string_literal: true

require 'set'

def build_grid
  input = $stdin.read
  lines = input.lines

  grid = {}

  lines.each_with_index do |row, y|
    row.strip.chars.each_with_index do |c, x|
      grid[[x, y]] = c
    end
  end

  [grid, lines.size]
end

grid, size = build_grid

def next_dir(space, dir)
  case [space, dir]
  when ['-', :north], ['-', :south] then %i[east west]
  when ['|', :east], ['|', :west] then %i[north south]
  when ['\\', :north] then [:west]
  when ['\\', :south] then [:east]
  when ['\\', :east] then [:south]
  when ['\\', :west] then [:north]
  when ['/', :north] then [:east]
  when ['/', :south] then [:west]
  when ['/', :east] then [:north]
  when ['/', :west] then [:south]
  else [dir]
  end
end

def step(point, dir)
  x, y = point

  case dir
  when :east then [x + 1, y]
  when :west then [x - 1, y]
  when :north then [x, y - 1]
  when :south then [x, y + 1]
  end
end

def walk(grid, start = [0, 0], direction = :east)
  visited = Hash.new { |h, k| h[k] = [] }

  queue = [[start, direction]]

  until queue.empty?
    pt, dir = queue.pop

    next if grid[pt].nil?
    next if visited[pt].include? dir

    visited[pt].push(dir)

    steps = next_dir(grid[pt], dir).map { |dir| [step(pt, dir), dir] }

    queue.concat(steps)
  end

  visited
end

def print_visited(_grid, visited, dims)
  (0...dims).each do |y|
    (0...dims).each do |x|
      pt = [x, y]

      if visited[pt].empty?
        print '.'
      else
        print '#'
      end
    end

    puts ''
  end
end

visited = walk(grid)

part_one = visited.count { |_k, v| !v.empty? }

puts "#{Time.now} part_one=#{part_one}"

easts = (0...size).map { |y| walk(grid, [0, y], :east).count { |_k, v| !v.empty? } }.max
wests = (0...size).map { |y| walk(grid, [size - 1, y], :west).count { |_k, v| !v.empty? } }.max
norths = (0...size).map { |x| walk(grid, [x, size - 1], :north).count { |_k, v| !v.empty? } }.max
souths = (0...size).map { |x| walk(grid, [x, 0], :south).count { |_k, v| !v.empty? } }.max

part_two = [easts, wests, norths, souths].max

puts "#{Time.now} part_two=#{part_two}"
