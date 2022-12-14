
def parsePacket(packetStr, startIndex)
    i = startIndex
    packet = []
    while i < packetStr.length do
        if packetStr[i] == '['
            (subPacket, i) = parsePacket(packetStr, i+1)
            packet.append(subPacket)
        elsif packetStr[i] == ']'
            return [packet, i+1]
        elsif packetStr[i] == ','
            i += 1
        else
            comma = packetStr.index(',',i)
            bracket = packetStr.index(']', i)
            if (comma != nil and (bracket == nil or comma < bracket))
                packet.append(Integer(packetStr[i, comma-i]))
                i = comma + 1
            elsif bracket != nil
                packet.append(Integer(packetStr[i, bracket-i]))
                return [packet, bracket+1]
            else
                i += 1
            end
        end
    end
    return [packet, packetStr.length]
end

# 0 => equals
# -1 => correct order
# 1 => incorrect order
def comparePackets(packet1, packet2)
    if packet1.class != Array
        packet1 = [packet1]
    end
    if packet2.class != Array
        packet2 = [packet2]
    end

    l = [packet1.length, packet2.length].min()
    i = 0
    while i < l do
        if packet1[i].class == Array or packet2[i].class == Array
            result = comparePackets(packet1[i], packet2[i])
            if result != 0
                return result
            end
        else
            if packet1[i] < packet2[i]
                return -1
            elsif packet1[i] > packet2[i]
                return 1
            end
        end
        i += 1
    end

    if packet1.length < packet2.length
        return -1
    elsif packet1.length > packet2.length
        return 1
    else
        return 0
    end
end


# Part 1
packetStr1 = nil
pair_index = 1
result = 0
File.readlines('input.txt').each do |line|
    if line.strip == ''
        packetStr1 = nil
        pair_index += 1
    elsif packetStr1 != nil
        (packet1, i) = parsePacket(packetStr1, 1)
        (packet2, i) = parsePacket(line.strip, 1)
        if comparePackets(packet1, packet2) == -1
            result += pair_index
        end
    else
        packetStr1 = line.strip
    end
end
puts result

# Part 2
packets = [[[2]], [[6]]]
File.readlines('input.txt').each do |line|
    if line.strip == ''
        packetStr1 = nil
    else
        packets.append(parsePacket(line.strip, 1))
    end
end

packets = packets.sort { |p1,p2| comparePackets(p1,p2) }

i = 0
divider1 = nil
divider2 = nil
while i < packets.length do
    p = packets[i]
    if p == [[2]]
        divider1 = i+1
    elsif p == [[6]]
        divider2 = i+1
    end
    i += 1
end
puts divider1*divider2